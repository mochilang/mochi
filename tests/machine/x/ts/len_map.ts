console.log((Array.isArray({a: 1, b: 2}) || typeof {a: 1, b: 2} === 'string' ? {a: 1, b: 2}.length : Object.keys({a: 1, b: 2}).length));
