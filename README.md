# Supertone

Supertone is a high-level music workstation focused on easy
composition of electronic tunes in a live setting.

**Not complete! See `supertone.studio.*` for improvements to the Overtone audio engine, fully-functional and ready-to-go. (For test code, check out the `dev` namespace.) MIDI/GUI code is just the basics, still needs work.**

## Installation

Make sure
[Leiningen](https://github.com/technomancy/leiningen) is installed, and clone this repository:
```sh
$ brew install leiningen

$ git clone https://github.com/nspotrepka/supertone.git
```

## Getting Started

To get started, start a new REPL session:
```sh
$ lein repl
```

When the REPL session has loaded, navigate to the `dev` namespace and go!
```clj
user=> (dev)

dev=> (go)
```

## Dependencies

[Clojure](https://clojure.org/)

[Overtone](https://github.com/overtone/overtone)

[Seesaw](https://github.com/daveray/seesaw)
