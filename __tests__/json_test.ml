open Jest

let () =
  let open Expect in
  let open Belt in
  let open Parser in
  let open Json in

  describe "jnull" (fun () ->
      test "success" (fun () ->
          let input = "null" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 4;
                line = 0;
              }
            } in

          expect (run jnull input)
          |> toEqual (Ok (Null, remaining)));

      test "failure" (fun () ->
          let input = "nulp" in

          expect ((run jnull input) |. Result.isError)
          |> toBe true);
    );

  describe "jbool" (fun () ->
      test "true" (fun () ->
          let input = "true" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 4;
                line = 0;
              }
            } in

          expect (run jbool input)
          |> toEqual (Ok (Bool true, remaining)));

      test "false" (fun () ->
          let input = "false" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 5;
                line = 0;
              }
            } in

          expect (run jbool input)
          |> toEqual (Ok (Bool false, remaining)));


      test "failure" (fun () ->
          let input = "nulp" in

          expect ((run jbool input) |. Result.isError)
          |> toBe true);
    );

  describe "junescaped_char" (fun () ->
      test "success" (fun () ->
          let input = "a" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 1;
                line = 0;
              }
            } in

          expect (run junescaped_char input)
          |> toEqual (Ok ("a", remaining)));


      test "failure" (fun () ->
          let input = "\\" in

          expect ((run junescaped_char input) |. Result.isError)
          |> toBe true);
    );

  describe "jescaped_char" (fun () ->
      test "success" (fun () ->
          let input = "\\\\" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 2;
                line = 0;
              }
            } in

          expect (run jescaped_char input)
          |> toEqual (Ok ("\\", remaining)));


      test "failure" (fun () ->
          let input = "a" in

          expect ((run jescaped_char input) |. Result.isError)
          |> toBe true);
    );

  describe "junicode_char" (fun () ->
      test "success" (fun () ->
          let input = "\\u263A" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 6;
                line = 0;
              }
            } in

          expect (run junicode_char input)
          |> toEqual (Ok ({j|\u263A|j}, remaining)));


      test "failure" (fun () ->
          let input = "a" in

          expect ((run junicode_char input) |. Result.isError)
          |> toBe true);
    );

  describe "jstring" (fun () ->
      test "empty" (fun () ->
          let input = "\"\"" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 2;
                line = 0;
              }
            } in

          expect (run jstring input)
          |> toEqual (Ok (String "", remaining)));

      test "single" (fun () ->
          let input = "\"a\"" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 3;
                line = 0;
              }
            } in

          expect (run jstring input)
          |> toEqual (Ok (String "a", remaining)));

      test "mix" (fun () ->
          let input = "\"ab\\tde\\u263Afg\"" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 16;
                line = 0;
              }
            } in

          expect (run jstring input)
          |> toEqual (Ok (String {j|ab\tde\u263Afg|j}, remaining)));


      test "failure" (fun () ->
          let input = "a" in

          expect ((run junicode_char input) |. Result.isError)
          |> toBe true);
    );

  describe "jnumber" (fun () ->
      test "integer part" (fun () ->
          let input = "1" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 1;
                line = 0;
              }
            } in

          expect (run jnumber input)
          |> toEqual (Ok (Number 1.0, remaining)));

      test "fractional part" (fun () ->
          let input = "1.4" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 3;
                line = 0;
              }
            } in

          expect (run jnumber input)
          |> toEqual (Ok (Number 1.4, remaining)));

      test "zero point" (fun () ->
          let input = "0.234" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 5;
                line = 0;
              }
            } in

          expect (run jnumber input)
          |> toEqual (Ok (Number 0.234, remaining)));

      test "exponent part" (fun () ->
          let input = "1e+23" in
          let remaining =
            {
              lines = [|input|];
              position = {
                column = 5;
                line = 0;
              }
            } in

          expect (run jnumber input)
          |> toEqual (Ok (Number 1e+23, remaining)));


    );

  describe "json" (fun () ->
      test "simple" (fun () ->
          let input = {|{
              "name": "Scott",
              "is_male": true,
              "bday": { "year": 2001, "month": 12, "day": 25 },
              "colors": ["blue", "green"]
            }|}
          in
          let ast =
            Object (MapStr.fromArray
                      [|
                        ("bday", Object (MapStr.fromArray
                                           [|
                                             ("day", Number 25.0);
                                             ("month", Number 12.0);
                                             ("year", Number 2001.0);
                                           |]
                                        ));
                        ("colors", Array [String "blue"; String "green"]);
                        ("is_male", Bool true);
                        ("name", String "Scott");
                      |]
                   )
          in
          let remaining =
            {
              lines = [|
                "{";
                {|              "name": "Scott",|};
                {|              "is_male": true,|};
                {|              "bday": { "year": 2001, "month": 12, "day": 25 },|};
                {|              "colors": ["blue", "green"]|};
                "            }";
              |];
              position = {
                column = 0;
                line = 6;
              }
            } in

          let actual = run jvalue input in
          expect (actual |. Result.isOk)
          |> toBe true);

    );
