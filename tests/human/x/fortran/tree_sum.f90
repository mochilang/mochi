program tree_sum
  implicit none
  type Tree
     logical :: isLeaf = .true.
     integer :: value = 0
     type(Tree), pointer :: left => null()
     type(Tree), pointer :: right => null()
  end type Tree

  type(Tree), target :: leaf
  type(Tree), target :: t, nodeRight

  ! Node right: Node(Leaf,2,Leaf)
  nodeRight%isLeaf = .false.
  nodeRight%value = 2
  nodeRight%left => leaf
  nodeRight%right => leaf

  ! root node: Node(Leaf,1,nodeRight)
  t%isLeaf = .false.
  t%value = 1
  t%left => leaf
  t%right => nodeRight

  print *, sum_tree(t)

contains
  recursive integer function sum_tree(node) result(res)
    type(Tree), intent(in) :: node
    if (node%isLeaf) then
      res = 0
    else
      res = sum_tree(node%left) + node%value + sum_tree(node%right)
    end if
  end function sum_tree
end program tree_sum
