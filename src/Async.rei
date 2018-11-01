type t('a) = ('a => unit) => unit;

let consume: (t('a), 'a => unit) => unit;

let value: 'a => t('a);

let map: (t('a), 'a => 'b) => t('b);

let flatMap: (t('a), 'a => t('b)) => t('b);

let tap: (t('a), 'a => unit) => t('a);

let both: (t('a), t('b)) => t(('a, 'b));

let all: list(t('a)) => t(list('a));

let pick: list(t('a)) => t('a);
