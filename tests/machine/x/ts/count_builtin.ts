console.log((Array.isArray([1, 2, 3]) || typeof [1, 2, 3] === 'string' ? [1, 2, 3].length : Object.keys([1, 2, 3]).length));
