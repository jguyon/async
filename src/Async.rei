open Belt;

type t('a) = ('a => unit) => unit;

let consume: (t('a), 'a => unit) => unit;

let subscribe: t('a) => t('a);

let value: 'a => t('a);

let map: (t('a), 'a => 'b) => t('b);

let flatMap: (t('a), 'a => t('b)) => t('b);

let tap: (t('a), 'a => unit) => t('a);

let both: (t('a), t('b)) => t(('a, 'b));

let all: list(t('a)) => t(list('a));

let pick: list(t('a)) => t('a);

type result('a, 'e) = t(Result.t('a, 'e));

let ok: 'a => result('a, _);

let error: 'e => result(_, 'e);

let mapOk: (result('a, 'e), 'a => 'b) => result('b, 'e);

let mapError: (result('a, 'e), 'e => 'f) => result('a, 'f);

let flatMapOk: (result('a, 'e), 'a => result('b, 'e)) => result('b, 'e);

let flatMapError: (result('a, 'e), 'e => result('a, 'f)) => result('a, 'f);

let tapOk: (result('a, 'e), 'a => unit) => result('a, 'e);

let tapError: (result('a, 'e), 'e => unit) => result('a, 'e);

let bothOk: (result('a, 'e), result('b, 'e)) => result(('a, 'b), 'e);

let allOk: list(result('a, 'e)) => result(list('a), 'e);
