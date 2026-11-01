Simple lexer library written in OCaml - can take POSIX regex strings, which can include bracket and optional expressions, and produce recognizers and tokenizers.
It doesn't do serialization so you can't save a generated NFA/DFA to a file to reuse, meaning they have to be computed every run. This should be fine for most
reasonable regexes.

The main interface is the Tokenizer.build_tokenizer function, which takes a list of (keyword-type, keyword) string pairs and a list of (token-type, regex) pairs
and returns a tokenizer which will max-munch tokenize a string via Tokenizer.tokenize_string tokenizer str, which returns a list of (TOKEN_TYPE; lexeme) tokens. If the lexeme of a token matches a keyword, it will be classified as that keyword type instead. There is one requirement which is that keywords must
otherwise be recognizable as some other token type, or the tokenizer will not recognize them - this is fine for most languages.

Individual NFAs/DFAs can also be built for a specific regex string via Regex.parse_regex_string and DFA.dfa_from_regex respectively. These can then be used to recognize strings via the NFA.nfa_accepts_str and DFA.dfa_accepts_str functions.