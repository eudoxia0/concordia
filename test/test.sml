open ParCom;
open Parser;
open CST;

val suite = Prover.suite;
val isEqual' = Prover.isEqual';

fun isParse input out = Prover.is (fn () => case (parseString input) of
                                                (Result res) => (res = out)
                                              | Fail _ => false)
                                  input
                                  "Bad parse";

fun ls l = SList ("b", NONE, l)

val test = suite "Concordia tests" [
        suite "Parser" [
            suite "Inline nodes" [
                isParse "a" (Text "a"),
                isParse "abc" (Text "abc"),
                isParse "\\b{}" (ls nil),
                isParse "\\b{\\b{}}" (ls [ls nil]),
                isParse "\\b{\\b{\\b{}}}" (ls [ls [ls nil]]),
                isParse "\\b{\\b{\\b{\\b{}}}}" (ls [ls [ls [ls nil]]]),
                isParse "\\b{\\b{a}}" (ls [ls [Text "a"]]),
                isParse "\\b{\\b{a} \\b{}}" (ls [ls [Text "a"], Text " ", ls nil]),
                isParse "\\b{a b c}" (ls [Text "a b c"]),
                isParse "\\b{a b \\b{c d} e f}" (ls [Text "a b ",
                                                     ls [Text "c d"],
                                                     Text " e f"]),
                suite "TeX" [
                    isParse "$tex$" (TeX "tex"),
                    isParse "$\\$$" (TeX "$"),
                    isParse "\\b{text and $tex$ and text}"
                            (ls [Text "text and ", TeX "tex", Text " and text"])
                ]
            ],
            suite "Inline nodes with argument" [
                isParse "\\link[text]{test}" (SList ("link", SOME "text", [Text "test"]))
            ]
        ],
        suite "Transform" [
            isEqual' (Transform.parseI (SList ("b", NONE, [Text "123"])))
                     (Document.Bold [Document.Text "123"])
        ]
];

Prover.runAndPrint test;
