function outer(x);
  function inner(y);
    return (x + y);
  end;
  return inner(5);
end;
print(outer(3));
