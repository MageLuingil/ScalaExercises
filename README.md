AdLib Parser
============

Parses MadLib-style input for replacement word tags, and replaces them with words randomly drawn from the
[WordNet](https://wordnet.princeton.edu/) database. 

ToDo
----

 * [x] Support tag IDs for re-using replacement words in source text
 * [ ] Accept input strings from CLI or files
   * [x] Accept filenames from CLI
   * [ ] Flesh out CLI interface
 * [ ] Add caching optimizations
 * [x] More granular hypernym control in tags
 * [ ] (Stretch goal) Auto-resolve articles and plurals to match the random word
