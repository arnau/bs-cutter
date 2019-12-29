open Jest

let () =
  let open Expect in
  let open Belt in
  let open Parser in

  describe "pchar" (fun () ->
      test "abc" (fun () ->
          let remaining =
            {
              lines = [|"ABC"|];
              position = {
                column = 1;
                line = 0;
              }
            } in

          expect (run (pchar "A") "ABC")
          |> toEqual (Result.Ok ("A", remaining)));

      test "zbc" (fun () ->
          let remaining =
            {
              current_line = "ZBC";
              column = 0;
              line = 0;
            } in
          expect (run (pchar "A") "ZBC")
          |> toEqual (Result.Error ("A", "Unexpected \"Z\"", remaining)));
    );

  describe "and_then" (fun () ->
      test "a then b" (fun () ->
          let parseAB =
            pchar "A"
            |. and_then (pchar "B")
            |. and_then (pchar "C")
          in
          let remaining =
            {
              lines = [|"ABC"|];
              position = {
                column = 3;
                line = 0;
              }
            } in
          expect (run (parseAB) "ABC")
          |> toEqual (Result.Ok ((("A", "B"), "C"), remaining)));

      test "a then b" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in
          let remaining =
            {
              lines = [|"ABC"|];
              position = {
                column = 2;
                line = 0;
              }
            } in
          expect (run (parseA >> parseB) "ABC")
          |> toEqual (Result.Ok (("A", "B"), remaining)));

    );

  describe "or_else" (fun () ->
      test "a or b" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in
          let remaining =
            {
              lines = [|"BCD"|];
              position = {
                column = 1;
                line = 0;
              }
            } in
          expect (run (parseA <|> parseB) "BCD")
          |> toEqual (Result.Ok ("B", remaining)));

      test "a" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in
          let remaining =
            {
              lines = [|"ABC"|];
              position = {
                column = 1;
                line = 0;
              }
            } in
          expect (run (parseA <|> parseB) "ABC")
          |> toEqual (Result.Ok ("A", remaining)));

    );

  describe "choice" (fun () ->
      test "use a" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in
          let parseC = pchar "C" in
          let parser = choice [|parseA; parseB; parseC|] in
          let remaining =
            {
              lines = [|"AZZ"|];
              position = {
                column = 1;
                line = 0;
              }
            } in

          expect (run parser "AZZ")
          |> toEqual (Result.Ok ("A", remaining)));

      test "use c" (fun () ->
          let parseA = pchar "A" in
          let parseB = pchar "B" in
          let parseC = pchar "C" in
          let parser = choice [|parseA; parseB; parseC|] in
          let remaining =
            {
              lines = [|"CZZ"|];
              position = {
                column = 1;
                line = 0;
              }
            } in

          expect (run parser "CZZ")
          |> toEqual (Result.Ok ("C", remaining)));

    );

  describe "map" (fun () ->
      test "use a" (fun () ->
          let parser = digit_char >> digit_char >> digit_char in
          let trans ((c1, c2), c3) =
            String.from_array [|c1; c2; c3|]
            |> Int.fromString
          in
          let remaining =
            {
              lines = [|"1234"|];
              position = {
                column = 3;
                line = 0;
              }
            } in
          expect (run (map parser trans) "1234")
          |> toEqual (Result.Ok (Some 123, remaining)));
    );

  describe "sequence" (fun () ->
      test "take abc" (fun () ->
          let parsers = [pchar "A"; pchar "B"; pchar "C"] in
          let abc = sequence parsers in
          let remaining =
            {
              lines = [|"ABCD"|];
              position = {
                column = 3;
                line = 0;
              }
            } in

          expect (run abc "ABCD")
          |> toEqual (Result.Ok (["A"; "B"; "C"], remaining)));
    );

  describe "pstring" (fun () ->
      test "take abc" (fun () ->
          let abc = pstring "ABC" in
          let remaining =
            {
              lines = [|"ABCD"|];
              position = {
                column = 3;
                line = 0;
              }
            } in
          expect (run abc "ABCD")
          |> toEqual (Result.Ok ("ABC", remaining)));

      test "fail abc" (fun () ->
          let abc = pstring "ABC" in

          expect (run abc "AB|D" |> Result.isError)
          |> toEqual true);

    );

  describe "many" (fun () ->
      test "many ab" (fun () ->
          let ab = many (pstring "AB") in
          let remaining =
            {
              lines = [|"ABABCD"|];
              position = {
                column = 4;
                line = 0;
              }
            } in
          expect (run ab "ABABCD")
          |> toEqual (Result.Ok (["AB"; "AB"], remaining)));

      test "no a" (fun () ->
          let parser = many (pchar "A") in
          let remaining =
            {
              lines = [|"BCD"|];
              position = {
                column = 0;
                line = 0;
              }
            } in
          expect (run parser "BCD")
          |> toEqual (Result.Ok ([], remaining)));

    );

  describe "many1" (fun () ->
      test "one or more number" (fun () ->
          let digit = any [| "1"; "2"; "3"; "4" |] in
          let digits = many1 digit in
          let remaining =
            {
              lines = [|"12AB"|];
              position = {
                column = 2;
                line = 0;
              }
            } in
          expect (run digits "12AB")
          |> toEqual (Result.Ok (["1"; "2"], remaining)));

      test "fail one or more number" (fun () ->
          let digit = any [| "1"; "2"; "3"; "4" |] in
          let digits = many1 digit in

          expect (run digits "ABC" |> Result.isError)
          |> toEqual true);

    );

  describe "pint" (fun () ->
      test "one digit" (fun () ->
          let remaining =
            {
              lines = [|"1AB"|];
              position = {
                column = 1;
                line = 0;
              }
            } in
          expect (run pint "1AB")
          |> toEqual (Result.Ok (1, remaining)));

      test "two digits" (fun () ->
          let remaining =
            {
              lines = [|"13AB"|];
              position = {
                column = 2;
                line = 0;
              }
            } in
          expect (run pint "13AB")
          |> toEqual (Result.Ok (13, remaining)));

      test "negative digit" (fun () ->
          let remaining =
            {
              lines = [|"-13AB"|];
              position = {
                column = 3;
                line = 0;
              }
            } in
          expect (run pint "-13AB")
          |> toEqual (Result.Ok (-13, remaining)));


      test "no digit" (fun () ->
          expect (run pint "ABC" |> Result.isError)
          |> toEqual true);
    );

  describe "pfloat" (fun () ->
      test "one digit" (fun () ->
          let remaining =
            {
              lines = [|"1.0"|];
              position = {
                column = 3;
                line = 0;
              }
            } in
          expect (run pfloat "1.0")
          |> toEqual (Result.Ok (1.0, remaining)));

      test "negative" (fun () ->
          let remaining =
            {
              lines = [|"-11.3AB"|];
              position = {
                column = 5;
                line = 0;
              }
            } in
          expect (run pfloat "-11.3AB")
          |> toEqual (Result.Ok (-11.3, remaining)));
    );


  describe "opt" (fun () ->
      test "digit with maybe a semicolon" (fun () ->
          let parser =
            pint
            |. and_drop (opt (pchar ";"))
          in
          let remaining =
            {
              lines = [|"1;"|];
              position = {
                column = 2;
                line = 0;
              }
            } in
          expect (run parser "1;")
          |> toEqual (Result.Ok (1, remaining)));

      test "digit with no semicolon" (fun () ->
          let remaining =
            {
              lines = [|"1A"|];
              position = {
                column = 1;
                line = 0;
              }
            } in
          expect (run (pint >! opt (pchar ";")) "1A")
          |> toEqual (Result.Ok (1, remaining)));

    );

  describe "drop" (fun () ->
      test "drop whitespace" (fun () ->
          let parser =
            (pstring "AB")
            |. and_drop (many1 whitespace_char)
            |. and_then (pstring "CD")
          in
          let remaining =
            {
              lines = [|"AB  CD"|];
              position = {
                column = 6;
                line = 0;
              }
            } in
          expect (run parser "AB  CD")
          |> toEqual (Result.Ok (("AB", "CD"), remaining)));

    );

  describe "between" (fun () ->
      test "quotes" (fun () ->
          let text str = between dquote_char (pstring str) dquote_char in
          let parser =
            (text "AB")
            |. and_drop (many1 whitespace_char)
            |. and_then (text "CD")
          in
          let remaining =
            {
              lines = [|"\"AB\"  \"CD\""|];
              position = {
                column = 10;
                line = 0;
              }
            } in
          expect (run parser "\"AB\"  \"CD\"")
          |> toEqual (Result.Ok (("AB", "CD"), remaining)));

    );

  describe "sepby" (fun () ->
      test "list of digit" (fun () ->
          let ws = many whitespace_char in
          let comma = between ws (pchar ",") ws in
          let parser =
            (sepby pint comma)
          in
          let remaining =
            {
              lines = [|"1, 2 ,   3,4"|];
              position = {
                column = 12;
                line = 0;
              }
            } in
          expect (run parser "1, 2 ,   3,4")
          |> toEqual (Result.Ok ([1; 2; 3; 4], remaining)));

      test "set of ints" (fun () ->
          let ws = many whitespace_char in
          let comma = between ws (pchar ",") ws in
          let obrace = pchar "{" |. and_drop ws in
          let cbrace = ws |. drop_and (pchar "}") in
          let parser =
            between obrace (sepby pint comma) cbrace
          in
          let remaining =
            {
              lines = [|"{ 1, 2, 3, 4 }"|];
              position = {
                column = 14;
                line = 0;
              }
            } in
          expect (run parser "{ 1, 2, 3, 4 }")
          |> toEqual (Result.Ok ([1; 2; 3; 4], remaining)));
    );

  describe "read_all_chars" (fun () ->
      test "empty" (fun () ->
          expect (from_str "" |. read_all_chars)
          |> toEqual []);

      test "single" (fun () ->
          expect (from_str "a" |. read_all_chars')
          |> toEqual ["a"; "\n"]);

      test "double" (fun () ->
          expect (from_str "ab" |. read_all_chars)
          |> toEqual ["a"; "b"; "\n"]);


      test "two lines" (fun () ->
          expect (from_str "a\nb" |. read_all_chars)
          |> toEqual ["a"; "\n"; "b"; "\n"]);


    );

  describe "print_result" (fun () ->
      test "error" (fun () ->
          let parser =
            pchar "A" >> pchar "B" <?> "AB"
          in
          expect (run parser "A|C" |. print_result)
          |> toEqual "Line:0 Col:1 Error parsing AB\nA|C\n ^Unexpected \"|\"");
    );
