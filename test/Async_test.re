open Jest;

let delay = (~ms=0, value, fin) => {
  let _ = Js.Global.setTimeout(() => fin(value), ms);
  ();
};

describe("Async", () => {
  open Expect;

  describe(".consume", () =>
    testAsync("runs the task", fin =>
      delay("value")
      ->Async.consume(value => expect(value) |> toEqual("value") |> fin)
    )
  );

  describe(".value", () =>
    testAsync("produces the given value", fin =>
      Async.value("value")
      ->Async.consume(value => expect(value) |> toEqual("value") |> fin)
    )
  );

  describe(".map", () =>
    testAsync("produces the created value", fin =>
      delay("42")
      ->Async.map(value => int_of_string(value))
      ->Async.consume(value => expect(value) |> toEqual(42) |> fin)
    )
  );

  describe(".flatMap", () =>
    testAsync("produces the value from the created task", fin =>
      delay("42")
      ->Async.flatMap(value => delay(int_of_string(value)))
      ->Async.consume(value => expect(value) |> toEqual(42) |> fin)
    )
  );

  describe(".tap", () => {
    testAsync("gives the produced value to the given function", fin => {
      let state = ref(None);

      delay("value")
      ->Async.tap(v => state := Some(v))
      ->Async.consume(_ => expect(state^) |> toEqual(Some("value")) |> fin);
    });

    testAsync("leaves the produced value unchanged", fin =>
      delay("value")
      ->Async.tap(_ => ())
      ->Async.consume(value => expect(value) |> toEqual("value") |> fin)
    );
  });
});