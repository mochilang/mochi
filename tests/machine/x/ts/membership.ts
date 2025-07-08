let nums = [1, 2, 3];
console.log((Array.isArray(nums) || typeof nums === 'string' ? nums.includes(2) : Object.prototype.hasOwnProperty.call(nums, 2)));
console.log((Array.isArray(nums) || typeof nums === 'string' ? nums.includes(4) : Object.prototype.hasOwnProperty.call(nums, 4)));
