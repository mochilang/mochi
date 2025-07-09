program dataset_sort_take_limit
  implicit none
  type :: Product
    character(len=20) :: name
    integer :: price
  end type Product
  type(Product) :: products(7)
  type(Product) :: sorted(7)
  integer :: i, j
  type(Product) :: temp

  products(1) = Product('Laptop',1500)
  products(2) = Product('Smartphone',900)
  products(3) = Product('Tablet',600)
  products(4) = Product('Monitor',300)
  products(5) = Product('Keyboard',100)
  products(6) = Product('Mouse',50)
  products(7) = Product('Headphones',200)

  sorted = products
  do i = 1, 6
    do j = i+1, 7
      if (sorted(j)%price > sorted(i)%price) then
        temp = sorted(i)
        sorted(i) = sorted(j)
        sorted(j) = temp
      end if
    end do
  end do

  print *, '--- Top products (excluding most expensive) ---'
  do i = 2, 4
    print *, trim(sorted(i)%name)//' costs $', sorted(i)%price
  end do
end program dataset_sort_take_limit
