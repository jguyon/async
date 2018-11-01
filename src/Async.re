open Belt;

type t('a) = ('a => unit) => unit;

let consume = (task: t('a), fin) => task(fin);

let value = (value, fin) => fin(value);

let map = (task: t('a), mapFn: 'a => 'b, fin) =>
  task(value => fin(mapFn(value)));

let flatMap = (task: t('a), flatMapFn: 'a => t('b), fin) =>
  task(value => flatMapFn(value, fin));

let tap = (task: t('a), tapFn: 'a => unit, fin) =>
  task(value => {
    tapFn(value);
    fin(value);
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
