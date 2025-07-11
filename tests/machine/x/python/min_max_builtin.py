nums = [3, 1, 4]
print(min([it for it in nums if it is not None]) if nums else 0)
print(max([it for it in nums if it is not None]) if nums else 0)
