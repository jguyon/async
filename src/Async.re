open Belt;

type t('a) = ('a => unit) => unit;

type result('a, 'e) = t(Result.t('a, 'e));

let consume = (task: t('a), fin) => task(fin);

type subscribeState('a) =
  | Pending(list('a => unit))
  | Finished('a);

let subscribe = (task: t('a)) => {
  let state = ref(Pending([]));

  task(value =>
    switch (state^) {
    | Finished(_) => ()
    | Pending(callbacks) =>
      state := Finished(value);
      List.reverse(callbacks)->List.forEach(cb => cb(value));
    }
  );

  fin =>
    switch (state^) {
    | Finished(value) => fin(value)
    | Pending(callbacks) => state := Pending([fin, ...callbacks])
    };
};

let value = (value, fin) => fin(value);

let ok = (value, fin) => fin(Result.Ok(value));

let error = (error, fin) => fin(Result.Error(error));

let map = (task: t('a), mapFn: 'a => 'b, fin) =>
  task(value => fin(mapFn(value)));

let mapOk = (task: result('a, 'e), mapFn: 'a => 'b, fin) =>
  task(
    fun
    | Error(_) as result => fin(result)
    | Ok(value) => fin(Ok(mapFn(value))),
  );

let mapError = (task: result('a, 'e), mapFn: 'e => 'f, fin) =>
  task(
    fun
    | Ok(_) as result => fin(result)
    | Error(error) => fin(Error(mapFn(error))),
  );

let flatMap = (task: t('a), flatMapFn: 'a => t('b), fin) =>
  task(value => flatMapFn(value, fin));

let flatMapOk = (task: result('a, 'e), flatMapFn: 'a => result('b, 'e), fin) =>
  task(
    fun
    | Error(_) as result => fin(result)
    | Ok(value) => flatMapFn(value, fin),
  );

let flatMapError =
    (task: result('a, 'e), flatMapFn: 'e => result('a, 'f), fin) =>
  task(
    fun
    | Ok(_) as result => fin(result)
    | Error(error) => flatMapFn(error, fin),
  );

let tap = (task: t('a), tapFn: 'a => unit, fin) =>
  task(value => {
    tapFn(value);
    fin(value);
  });

let tapOk = (task: result('a, 'e), tapFn: 'a => unit, fin) =>
  task(result => {
    switch (result) {
    | Error(_) => ()
    | Ok(value) => tapFn(value)
    };
    fin(result);
  });

let tapError = (task: result('a, 'e), tapFn: 'e => unit, fin) =>
  task(result => {
    switch (result) {
    | Ok(_) => ()
    | Error(error) => tapFn(error)
    };
    fin(result);
  });

type bothState('a, 'b) =
  | Empty
  | Left('a)
  | Right('b)
  | Finished;

let both = (leftTask: t('a), rightTask: t('b), fin) => {
  let state = ref(Empty);

  leftTask(leftValue =>
    switch (state^) {
    | Finished
    | Left(_) => ()
    | Empty => state := Left(leftValue)
    | Right(rightValue) =>
      state := Finished;
      fin((leftValue, rightValue));
    }
  );

  rightTask(rightValue =>
    switch (state^) {
    | Finished
    | Right(_) => ()
    | Empty => state := Right(rightValue)
    | Left(leftValue) =>
      state := Finished;
      fin((leftValue, rightValue));
    }
  );
};

let bothOk = (leftTask: result('a, 'e), rightTask: result('b, 'e), fin) => {
  let state = ref(Empty);

  leftTask(leftResult =>
    switch (state^, leftResult) {
    | (Finished | Left(_), _) => ()
    | (Empty | Right(_), Error(error)) =>
      state := Finished;
      fin(Result.Error(error));
    | (Empty, Ok(leftValue)) => state := Left(leftValue)
    | (Right(rightValue), Ok(leftValue)) =>
      state := Finished;
      fin(Result.Ok((leftValue, rightValue)));
    }
  );

  rightTask(rightResult =>
    switch (state^, rightResult) {
    | (Finished | Right(_), _) => ()
    | (Empty | Left(_), Error(error)) =>
      state := Finished;
      fin(Result.Error(error));
    | (Empty, Ok(rightValue)) => state := Right(rightValue)
    | (Left(leftValue), Ok(rightValue)) =>
      state := Finished;
      fin(Result.Ok((leftValue, rightValue)));
    }
  );
};

type allState('a) =
  | Pending(int, array(option('a)))
  | Finished;

let all = (tasks: list(t('a)), fin) => {
  let length = List.length(tasks);

  if (length == 0) {
    fin([]);
  } else {
    let state = ref(Pending(0, Array.make(length, None)));

    List.forEachWithIndex(tasks, (i, task) =>
      task(value =>
        switch (state^) {
        | Finished => ()
        | Pending(resolved, values) =>
          switch (Array.getUnsafe(values, i)) {
          | Some(_) => ()
          | None =>
            Array.setUnsafe(values, i, Some(value));
            let resolved = resolved + 1;
            if (resolved == length) {
              state := Finished;
              fin(
                List.makeBy(length, i =>
                  Array.getUnsafe(values, i)->Option.getExn
                ),
              );
            } else {
              state := Pending(resolved, values);
            };
          }
        }
      )
    );
  };
};

let allOk = (tasks: list(result('a, 'e)), fin) => {
  let length = List.length(tasks);

  if (length == 0) {
    fin(Result.Ok([]));
  } else {
    let state = ref(Pending(0, Array.make(length, None)));

    List.forEachWithIndex(tasks, (i, task) =>
      task(result =>
        switch (state^) {
        | Finished => ()
        | Pending(resolved, values) =>
          switch (Array.getUnsafe(values, i), result) {
          | (Some(_), _) => ()
          | (None, Error(error)) =>
            state := Finished;
            fin(Result.Error(error));
          | (None, Ok(value)) =>
            Array.setUnsafe(values, i, Some(value));
            let resolved = resolved + 1;
            if (resolved == length) {
              state := Finished;
              fin(
                Result.Ok(
                  List.makeBy(length, i =>
                    Array.getUnsafe(values, i)->Option.getExn
                  ),
                ),
              );
            } else {
              state := Pending(resolved, values);
            };
          }
        }
      )
    );
  };
};

type pickState =
  | Pending
  | Finished;

let pick = (tasks, fin) => {
  let state = ref(Pending);

  let callback = value =>
    switch (state^) {
    | Finished => ()
    | Pending =>
      state := Finished;
      fin(value);
    };

  List.forEach(tasks, task => task(callback));
};
