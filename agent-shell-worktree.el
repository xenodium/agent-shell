;;; agent-shell-worktree.el --- Git worktree support for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Git worktree support for agent-shell.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'subr-x)

(declare-function agent-shell "agent-shell")
(declare-function agent-shell--dot-subdir "agent-shell")

;; Word lists for generating random worktree names (Docker-style naming)
(defconst agent-shell-worktree--adjectives
  '("admiring" "adoring" "affectionate" "agitated" "amazing" "angry" "awesome"
    "beautiful" "blissful" "bold" "boring" "brave" "busy" "charming" "clever"
    "cool" "compassionate" "competent" "condescending" "confident" "cranky"
    "crazy" "dazzling" "determined" "distracted" "dreamy" "eager" "ecstatic"
    "elastic" "elated" "elegant" "eloquent" "epic" "exciting" "fervent"
    "festive" "flamboyant" "focused" "friendly" "frosty" "funny" "gallant"
    "gifted" "goofy" "gracious" "great" "happy" "hardcore" "heuristic"
    "hopeful" "hungry" "infallible" "inspiring" "intelligent" "interesting"
    "jolly" "jovial" "keen" "kind" "laughing" "loving" "lucid" "magical"
    "mystifying" "modest" "musing" "naughty" "nervous" "nice" "nifty"
    "nostalgic" "objective" "optimistic" "peaceful" "pedantic" "pensive"
    "practical" "priceless" "quirky" "quizzical" "recursing" "relaxed"
    "reverent" "romantic" "sad" "serene" "sharp" "silly" "sleepy" "stoic"
    "strange" "stupefied" "suspicious" "sweet" "tender" "thirsty" "trusting"
    "unruffled" "upbeat" "vibrant" "vigilant" "vigorous" "wizardly" "wonderful"
    "xenodochial" "youthful" "zealous" "zen")
  "Adjectives for generating random worktree names.")

(defconst agent-shell-worktree--scientists
  '("albattani" "allen" "almeida" "antonelli" "agnesi" "archimedes" "ardinghelli"
    "aryabhata" "austin" "babbage" "banach" "banzai" "bardeen" "bartik" "bassi"
    "beaver" "bell" "benz" "bhabha" "bhaskara" "blackburn" "blackwell" "bohr"
    "booth" "borg" "bose" "bouman" "boyd" "brahmagupta" "brattain" "brown"
    "buck" "burnell" "cannon" "carson" "cartwright" "carver" "cerf" "chandrasekhar"
    "chaplygin" "chatelet" "chatterjee" "chebyshev" "clarke" "colden" "cori"
    "cray" "curie" "darwin" "davinci" "dewdney" "dhawan" "diffie" "dijkstra"
    "dirac" "driscoll" "dubinsky" "easley" "edison" "einstein" "elbakyan"
    "elgamal" "elion" "ellis" "engelbart" "euclid" "euler" "faraday" "feistel"
    "fermat" "fermi" "feynman" "franklin" "gagarin" "galileo" "galois" "ganguly"
    "gates" "gauss" "germain" "goldberg" "goldstine" "goldwasser" "golick"
    "goodall" "gould" "greider" "grothendieck" "haibt" "hamilton" "haslett"
    "hawking" "heisenberg" "hellman" "hermann" "herschel" "hertz" "heyrovsky"
    "hodgkin" "hofstadter" "hoover" "hopper" "hugle" "hypatia" "jackson"
    "jang" "jemison" "jennings" "jepsen" "johnson" "joliot" "jones" "kalam"
    "kapitsa" "kare" "keldysh" "keller" "kepler" "khorana" "kilby" "kirch"
    "knuth" "kowalevski" "lamport" "leakey" "leavitt" "lederberg" "lehmann"
    "lewin" "lichterman" "liskov" "lovelace" "lumiere" "mahavira" "margulis"
    "matsumoto" "maxwell" "mayer" "mccarthy" "mcclintock" "mclaren" "mclean"
    "mcnulty" "meitner" "mendel" "mendeleev" "minsky" "mirzakhani" "montalcini"
    "moore" "morse" "murdock" "moser" "napier" "nash" "neumann" "newton"
    "nightingale" "nobel" "noether" "northcutt" "noyce" "panini" "pare"
    "pascal" "pasteur" "payne" "perlman" "pike" "poincare" "poitras" "proskuriakova"
    "ptolemy" "raman" "ramanujan" "ride" "ritchie" "rhodes" "robinson" "roentgen"
    "rosalind" "rubin" "saha" "sammet" "sanderson" "satoshi" "shamir" "shannon"
    "shaw" "shirley" "shockley" "shtern" "sinoussi" "snyder" "solomon" "spence"
    "stonebraker" "sutherland" "swanson" "swartz" "swirles" "taussig" "tereshkova"
    "tesla" "tharp" "thompson" "torvalds" "tu" "turing" "varahamihira" "vaughan"
    "villani" "visvesvaraya" "volhard" "wescoff" "wilbur" "wiles" "williams"
    "williamson" "wilson" "wing" "wozniak" "wright" "wu" "yalow" "yonath" "zhukovsky")
  "Scientists and notable figures for generating random worktree names.")

(defun agent-shell-worktree--generate-name ()
  "Generate a random worktree name using adjective-scientist format.
Returns a string like \"adoring-hawking\" or \"focused-turing\"."
  (format "%s-%s"
          (nth (random (length agent-shell-worktree--adjectives))
               agent-shell-worktree--adjectives)
          (nth (random (length agent-shell-worktree--scientists))
               agent-shell-worktree--scientists)))

(defun agent-shell-worktree--git-repo-root ()
  "Return the root directory of the current git repository.
Or nil if not in a repo."
  (when-let* ((output (shell-command-to-string
                       "git rev-parse --show-toplevel 2>/dev/null")))
    (let ((trimmed (string-trim output)))
      (unless (string-empty-p trimmed)
        trimmed))))

;;;###autoload
(defun agent-shell-new-worktree-shell ()
  "Create a new git worktree and start an agent shell in it.

The worktree is created at <project-root>/.agent-shell/worktrees/<worktree-name>
where <worktree-name> is a randomly generated name (e.g., \"adoring-hawking\").

The user is prompted to confirm or edit the worktree path before creation."
  (interactive)
  (unless (agent-shell-worktree--git-repo-root)
    (user-error "Not in a git repository"))
  (let* ((worktrees-dir (agent-shell--dot-subdir "worktrees"))
         (worktree-name (agent-shell-worktree--generate-name))
         (default-path (file-name-concat worktrees-dir worktree-name))
         (worktree-path (expand-file-name (read-directory-name
                                           "Worktree directory: "
                                           default-path))))
    (when (file-exists-p worktree-path)
      (user-error "Directory already exists: %s" worktree-path))
    (make-directory (file-name-directory worktree-path) t)
    ;; Create the worktree
    (let ((output (shell-command-to-string
                   (format "git worktree add %s 2>&1"
                           (shell-quote-argument worktree-path)))))
      (unless (file-exists-p worktree-path)
        (user-error "Failed to create worktree: %s" output))
      (let ((default-directory worktree-path))
        (agent-shell '(4))))))

(provide 'agent-shell-worktree)

;;; agent-shell-worktree.el ends here
