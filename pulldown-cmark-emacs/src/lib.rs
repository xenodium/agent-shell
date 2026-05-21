use emacs::{defun, Env, Result, Value, IntoLisp};
use pulldown_cmark::{Event, Options, Parser, Tag};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "pulldown-cmark-emacs")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

/// Helper function to convert a byte index into a UTF-8 character index
fn byte_to_char_idx(s: &str, byte_idx: usize) -> usize {
    s[..byte_idx].chars().count()
}

/// Parses a markdown string and returns a list of plists containing block/span locations.
/// Format: ((:type "paragraph" :start 0 :end 15) (:type "strong" :start 6 :end 12))
#[defun]
fn parse(env: &Env, text: String) -> Result<Value<'_>> {
    let mut options = Options::empty();
    options.insert(Options::ENABLE_STRIKETHROUGH);
    options.insert(Options::ENABLE_SUPERSCRIPT);
    options.insert(Options::ENABLE_SUBSCRIPT);
    let parser = Parser::new_ext(&text, options);
    let mut elements = Vec::new();

    let kw_type = env.intern(":type")?;
    let kw_start = env.intern(":start")?;
    let kw_end = env.intern(":end")?;

    for (event, range) in parser.into_offset_iter() {
        let type_name = match event {
            Event::Start(tag) => match tag {
                Tag::Paragraph => None,
                Tag::Strong => Some(":strong"),
                Tag::Emphasis => Some(":emphasis"),
                Tag::Strikethrough => Some(":strikethrough"),
                Tag::Superscript => Some(":superscript"),
                Tag::Subscript => Some(":subscript"),
                Tag::Heading { .. } => Some(":heading"),
                Tag::CodeBlock(_) => Some(":code-block"),
                Tag::Link { .. } => Some(":link"),
                Tag::Item => Some(":list"),
                _ => None
            },
            Event::Code(_) => Some(":code"),
            _ => None,
        };

        if let Some(name) = type_name {
            let start_char = byte_to_char_idx(&text, range.start);
            let end_char = byte_to_char_idx(&text, range.end);

            let plist = env.list(&[
                kw_type,
                env.intern(name)?,
                kw_start,
                start_char.into_lisp(env)?,
                kw_end,
                end_char.into_lisp(env)?,
            ])?;

            elements.push(plist);
        }
    }

    env.list(&elements)
}
