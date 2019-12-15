open Jest

let () =
  let open Expect in
  let open Belt in
  let open Parser in

  describe "pchar" (fun () ->
      test "abc" (fun () ->
          expect (run (pchar "A") "ABC")
          |> toEqual (Result.Ok ("A", "BC")));

      test "zbc" (fun () ->
          expect (run (pchar "A") "ZBC")
          |> toEqual (Result.Error "Expecting A. Got Z"));

    );

  describe "and_then" (fun () ->
      test "a then b" (fun () ->
          let parseAB =
            pchar "A"
            |. and_then (pchar "B")
            |. and_then (pchar "C")
          in
          expect (run (parseAB) "ABC")
          |> toEqual (Result.Ok ((("A", "B"), "C"), "")));

      test "a then b" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in
          expect (run (parseA >> parseB) "ABC")
          |> toEqual (Result.Ok (("A", "B"), "C")));

    );

  describe "or_else" (fun () ->
      test "a or b" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in

          expect (run (parseA <|> parseB) "BCD")
          |> toEqual (Result.Ok ("B", "CD")));

      test "a" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in
          expect (run (parseA <|> parseB) "ABC")
          |> toEqual (Result.Ok ("A", "BC")));

    );

  describe "choice" (fun () ->
      test "use a" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in
          let parseC = pchar "C" in
          let parser = choice [|parseA; parseB; parseC|] in

          expect (run parser "AZZ")
          |> toEqual (Result.Ok ("A", "ZZ")));

      test "use c" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in
          let parseC = pchar "C" in
          let parser = choice [|parseA; parseB; parseC|] in

          expect (run parser "CZZ")
          |> toEqual (Result.Ok ("C", "ZZ")));

    );

  describe "map" (fun () ->
      test "use a" (fun () ->
          let pdigit = any [|"1"; "2"; "3"|] in
          let parser = pdigit >> pdigit >> pdigit in
          let trans ((c1, c2), c3) =
            String.from_array [|c1; c2; c3|]
            |> Int.fromString
          in

          expect (run (map parser trans) "1234")
          |> toEqual (Result.Ok (Some 123, "4")));
    );

  describe "sequence" (fun () ->
      test "take abc" (fun () ->
          let parsers = [pchar "A"; pchar "B"; pchar "C"] in
          let abc = sequence parsers in

          expect (run abc "ABCD")
          |> toEqual (Result.Ok (["A"; "B"; "C"], "D")));
    );

  describe "pstring" (fun () ->
      test "take abc" (fun () ->
          let abc = pstring "ABC" in

          expect (run abc "ABCD")
          |> toEqual (Result.Ok ("ABC", "D")));

      test "fail abc" (fun () ->
          let abc = pstring "ABC" in

          expect (run abc "AB|D" |> Result.isError)
          |> toEqual true);

    );

  describe "many" (fun () ->
      test "many ab" (fun () ->
          let ab = many (pstring "AB") in

          expect (run ab "ABABCD")
          |> toEqual (Result.Ok (["AB"; "AB"], "CD")));

      test "no a" (fun () ->
          let parser = many (pchar "A") in

          expect (run parser "BCD")
          |> toEqual (Result.Ok ([], "BCD")));

    );

  describe "many1" (fun () ->
      test "one or more number" (fun () ->
          let digit = any [| "1"; "2"; "3"; "4" |] in
          let digits = many1 digit in

          expect (run digits "12AB")
          |> toEqual (Result.Ok (["1"; "2"], "AB")));

      test "fail one or more number" (fun () ->
          let digit = any [| "1"; "2"; "3"; "4" |] in
          let digits = many1 digit in

          expect (run digits "ABC" |> Result.isError)
          |> toEqual true);

    );

  describe "pint" (fun () ->
      test "one digit" (fun () ->
          expect (run pint "1AB")
          |> toEqual (Result.Ok (1, "AB")));

      test "two digits" (fun () ->
          expect (run pint "13AB")
          |> toEqual (Result.Ok (13, "AB")));

      test "negative digit" (fun () ->
          expect (run pint "-13AB")
          |> toEqual (Result.Ok (-13, "AB")));


      test "no digit" (fun () ->
          expect (run pint "ABC" |> Result.isError)
          |> toEqual true);
    );

  describe "opt" (fun () ->
      test "digit with maybe a semicolon" (fun () ->
          let parser =
            pint
            |. and_drop (opt (pchar ";"))
          in
          expect (run parser "1;")
          |> toEqual (Result.Ok (1, "")));

      test "digit with no semicolon" (fun () ->
          expect (run (pint >! opt (pchar ";")) "1A")
          |> toEqual (Result.Ok (1, "A")));

    );

  describe "drop" (fun () ->
      test "drop whitespace" (fun () ->
          let whitespace = any [|" "; "\t"; "\n"|] in
          let parser =
            (pstring "AB")
            |. and_drop (many1 whitespace)
            |. and_then (pstring "CD")
          in
          expect (run parser "AB  CD")
          |> toEqual (Result.Ok (("AB", "CD"), "")));

    );

  describe "between" (fun () ->
      test "quotes" (fun () ->
          let whitespace = any [|" "; "\t"; "\n"|] in
          let quote = pchar "\"" in
          let text str = between quote (pstring str) quote in
          let parser =
            (text "AB")
            |. and_drop (many1 whitespace)
            |. and_then (text "CD")
          in
          expect (run parser "\"AB\"  \"CD\"")
          |> toEqual (Result.Ok (("AB", "CD"), "")));

    );

  describe "sepby" (fun () ->
      test "list of digit" (fun () ->
          let ws = any [|" "; "\t"; "\n"|] in
          let comma = between (many ws) (pchar ",") (many ws) in
          let parser =
            (sepby pint comma)
          in
          expect (run parser "1, 2 ,   3,4")
          |> toEqual (Result.Ok ([1; 2; 3; 4], "")));

      test "set of ints" (fun () ->
          let ws = any [|" "; "\t"; "\n"|] in
          let comma = between (many ws) (pchar ",") (many ws) in
          let obrace = pchar "{" |. and_drop (many ws) in
          let cbrace = (many ws) |. drop_and (pchar "}") in
          let parser =
            between obrace (sepby pint comma) cbrace
          in
          expect (run parser "{ 1, 2 , 3, 4 }")
          |> toEqual (Result.Ok ([1; 2; 3; 4], "")));
    );
