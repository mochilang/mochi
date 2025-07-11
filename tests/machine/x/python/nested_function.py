def outer(x):

    def inner(y):
        return x + y

    return inner(5)


print(outer(3))
