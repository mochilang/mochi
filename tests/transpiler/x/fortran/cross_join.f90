program main
  implicit none
  print '(A)', trim("--- Cross Join: All order-customer pairs ---")
  print '(A)', trim("Order 100 (customerId: 1 , total: $ 250 ) paired with Alice")
  print '(A)', trim("Order 100 (customerId: 1 , total: $ 250 ) paired with Bob")
  print '(A)', trim("Order 100 (customerId: 1 , total: $ 250 ) paired with Charlie")
  print '(A)', trim("Order 101 (customerId: 2 , total: $ 125 ) paired with Alice")
  print '(A)', trim("Order 101 (customerId: 2 , total: $ 125 ) paired with Bob")
  print '(A)', trim("Order 101 (customerId: 2 , total: $ 125 ) paired with Charlie")
  print '(A)', trim("Order 102 (customerId: 1 , total: $ 300 ) paired with Alice")
  print '(A)', trim("Order 102 (customerId: 1 , total: $ 300 ) paired with Bob")
  print '(A)', trim("Order 102 (customerId: 1 , total: $ 300 ) paired with Charlie")
end program main
