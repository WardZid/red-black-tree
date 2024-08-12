
red = False
black = True

class Node:
    def __init__(self, key, color=red, parent=None, left=None, right=None, black_height=None, is_nil=False):
        self.key = key
        self.color = color
        self.parent = parent
        self.left = left
        self.right = right
        self.black_height = black_height if black_height is not None else 0
        self.is_nil = is_nil


        if left is not None and not left.is_nil:
            left.parent = self
        if right is not None and not right.is_nil:
            right.parent = self

        if black_height is None:
            self.fix_black_height()

    def __str__(self):
        return f"{self.key}({self.print_color()})"

    def fix_black_height(self):
        if self.left is not None or self.right is not None:
            if self.left is None:
                self.black_height = self.right.black_height
            elif self.right is None:
                self.black_height = self.left.black_height
            else:
                self.black_height = max(self.left.black_height, self.right.black_height)

        self.black_height += 1 if self.color == black and not self.is_nil else 0

    def print_color(self):
        if self.color == red:
            return "red"
        return "black"

    def print_subtree(self, start_node=None):
        def print_helper(node=None, indent="", last=True):
            if node != RedBlackTree.NULL:
                print(indent, end="")
                if last:
                    print("R----", end="")
                    indent += "     "
                else:
                    print("L----", end="")
                    indent += "|    "
                color = "RED" if not node.color else "BLACK"
                print(f"{node.key}({color}) bh:{node.black_height}")
                print_helper(node.left, indent, False)
                print_helper(node.right, indent, True)

        if start_node is None:
            print_helper(self)
        else:
            print_helper(start_node)

    @staticmethod
    def rank(node):
        if node is None or node == RedBlackTree.NULL:
            return 0
        if node.color == black:
            return 2 * (node.black_height - 1)
        return (2 * node.black_height) - 1

    @staticmethod
    def expose(node):
        if node is None:
            return RedBlackTree.NULL, 0, RedBlackTree.NULL
        return node.left, node.key, node.right

class RedBlackTree:
    NULL = Node(0, black, is_nil=True)

    def __init__(self, root=NULL):
        self.root = root

    def __str__(self):
        if self.root == self.NULL:
            print("Empty Tree")
        else:
            self.root.print_subtree()
        return ""

    def search(self, key):
        search_result = self._search_helper(self.root, key)
        if search_result == self.NULL:
            return None
        return search_result

    @staticmethod
    def _search_helper(node, key):
        if node == RedBlackTree.NULL:
            return RedBlackTree.NULL

        if node.key == key:
            return node

        if key < node.key:
            return RedBlackTree._search_helper(node.left, key)
        return RedBlackTree._search_helper(node.right, key)

    def _right_rotate(self, node):
        left_child = node.left
        node.left = left_child.right

        if left_child.right != self.NULL:
            left_child.right.parent = node

        left_child.parent = node.parent

        if node.parent == self.NULL:
            self.root = left_child
        elif node == node.parent.right:
            node.parent.right = left_child
        else:
            node.parent.left = left_child

        left_child.right = node
        node.parent = left_child

    def _left_rotate(self, node):
        right_child = node.right
        node.right = right_child.left

        if right_child.left != self.NULL:
            right_child.left.parent = node

        right_child.parent = node.parent

        if node.parent == self.NULL:
            self.root = right_child
        elif node == node.parent.left:
            node.parent.left = right_child
        else:
            node.parent.right = right_child

        right_child.left = node
        node.parent = right_child

    def insert(self, new_val):
        new_node = Node(new_val)
        new_node.parent = self.NULL
        new_node.left = self.NULL
        new_node.right = self.NULL

        if self.root == self.NULL:
            self.root = new_node
            self.root.color = black
            self.root.black_height = 1
            return True


        current = self.root
        parent = self.NULL

        while current != self.NULL:
            parent = current
            if new_node.key < current.key:
                current = current.left
            elif new_node.key > current.key:
                current = current.right
            else:
                # print("duplicates not supported")
                return False

        # now current is NULL

        # give parent new node as child accordingly. left/right
        if new_node.key < parent.key:
            parent.left = new_node
        else:
            parent.right = new_node

        new_node.parent = parent

        # insertion done, now to balance
        node_to_balance = new_node

        while node_to_balance.parent is not self.NULL and node_to_balance.parent.color == red:
            grandparent = node_to_balance.parent.parent
            left_uncle = grandparent.left
            right_uncle = grandparent.right

            parent_is_left_uncle = (node_to_balance.parent == left_uncle)
            if parent_is_left_uncle:
                if right_uncle.color == red:
                    node_to_balance.parent.color = black
                    right_uncle.color = black
                    node_to_balance.parent.parent.color = red #grandparent.color = red

                    node_to_balance.parent.black_height += 1
                    right_uncle.black_height += 1

                    node_to_balance = node_to_balance.parent.parent #grandparent
                else:
                    is_left_triangle = (node_to_balance == node_to_balance.parent.right)
                    if is_left_triangle:
                        node_to_balance = node_to_balance.parent
                        self._left_rotate(node_to_balance)
                    node_to_balance.parent.color = black
                    node_to_balance.parent.parent.color = red

                    node_to_balance.parent.black_height += 1
                    node_to_balance.parent.parent.black_height -= 1

                    self._right_rotate(node_to_balance.parent.parent)
            else:
                if left_uncle.color == red:
                    node_to_balance.parent.color = black
                    left_uncle.color = black
                    node_to_balance.parent.parent.color = red  # grandparent.color = red

                    node_to_balance.parent.black_height += 1
                    left_uncle.black_height += 1

                    node_to_balance = node_to_balance.parent.parent
                else:
                    is_right_triangle = (node_to_balance == node_to_balance.parent.left)
                    if is_right_triangle:
                        node_to_balance = node_to_balance.parent
                        self._right_rotate(node_to_balance)
                    node_to_balance.parent.color = black
                    node_to_balance.parent.parent.color = red

                    node_to_balance.parent.black_height += 1
                    node_to_balance.parent.parent.black_height -= 1

                    self._left_rotate(node_to_balance.parent.parent)

        if self.root.color == red:
            self.root.color = black
            self.root.black_height += 1
        return True

    def _transplant(self, old_node, new_node):
        if old_node.parent == self.NULL:
            self.root = new_node
        elif old_node == old_node.parent.left:
            old_node.parent.left = new_node
        else:
            old_node.parent.right = new_node
        new_node.parent = old_node.parent

    def _minimum(self, start_node):
        if start_node == self.NULL:
            return None

        min_node = start_node
        while min_node.left != self.NULL:
            min_node = min_node.left
        return min_node

    def _maximum(self, start_node):
        if start_node == self.NULL:
            return None

        max_node = start_node
        while max_node.right != self.NULL:
            max_node = max_node.right
        return max_node

    def remove(self, key):
        node_to_remove = self.search(key)

        if node_to_remove is None or node_to_remove == self.NULL:
            return False

        if node_to_remove.left == self.NULL:
            replacement_node = node_to_remove.right
            self._transplant(node_to_remove, node_to_remove.right)
        elif node_to_remove.right == self.NULL:
            replacement_node = node_to_remove.left
            self._transplant(node_to_remove, node_to_remove.left)
        else:
            min_right_node = self._minimum(node_to_remove.right)

            replacement_node = min_right_node.right
            self._transplant(min_right_node, min_right_node.right)
            self._transplant(node_to_remove, min_right_node)

            min_right_node.right = node_to_remove.right
            node_to_remove.right.parent = min_right_node

            min_right_node.left = node_to_remove.left
            node_to_remove.left.parent = min_right_node

            min_right_node.color = node_to_remove.color
            min_right_node.black_height = node_to_remove.black_height

        black_depth_violated = (node_to_remove.color == black)
        if black_depth_violated:
            node_to_fix = replacement_node
            while node_to_fix != self.root and node_to_fix.color == black:
                is_left_child = (node_to_fix == node_to_fix.parent.left)
                if is_left_child:
                    sibling = node_to_fix.parent.right

                    if sibling.color == red:
                        sibling.color = black
                        node_to_fix.parent.color = red

                        sibling.black_height += 1
                        node_to_fix.parent -= 1

                        self._left_rotate(node_to_fix.parent)
                        sibling = node_to_fix.parent.right

                    if sibling.left.color == black and sibling.right.color == black:
                        sibling.color = red
                        sibling.black_height -= 1
                        node_to_fix = node_to_fix.parent
                    else:
                        if sibling.right.color == black:
                            sibling.left.color = black
                            sibling.color = red

                            sibling.left.black_height += 1
                            sibling.black_height -= 1

                            self._right_rotate(sibling)
                            sibling = node_to_fix.parent.right

                        sibling.color = node_to_fix.parent.color
                        node_to_fix.parent.color = black
                        sibling.right.color = black

                        node_to_fix.parent.black_height += 1
                        sibling.right.black_height += 1

                        self._left_rotate(node_to_fix.parent)
                        node_to_fix = self.root
                else:
                    sibling = node_to_fix.parent.left

                    if sibling.color == red:
                        sibling.color = black
                        node_to_fix.parent.color = red

                        sibling.black_height += 1
                        node_to_fix.parent -= 1

                        self._right_rotate(node_to_fix.parent)
                        sibling = node_to_fix.parent.left

                    if sibling.left.color == black and sibling.right.color == black:
                        sibling.color = red
                        sibling.black_height -= 1
                        node_to_fix = node_to_fix.parent
                    else:
                        if sibling.left.color == black:
                            sibling.right.color = black
                            sibling.color = red

                            sibling.right.black_height += 1
                            sibling.black_height -= 1

                            self._left_rotate(sibling)
                            sibling = node_to_fix.parent.left

                        sibling.color = node_to_fix.parent.color
                        node_to_fix.parent.color = black
                        sibling.left.color = black
                        self._right_rotate(node_to_fix.parent)
                        node_to_fix = self.root
            node_to_fix.color = black
        return True

    def delete(self, key):
        left, exists, right = RedBlackTree.split(self.root, key)
        self.root = RedBlackTree.join2(left, right)

    def values(self):
        def inorder_helper(node, result):
            if node != self.NULL:
                inorder_helper(node.left, result)
                result.append(node.key)
                inorder_helper(node.right, result)
        res = []
        inorder_helper(self.root, res)
        return res

    @staticmethod
    def _join_right(left_tree, key, right_tree):

        if left_tree is None or left_tree == RedBlackTree.NULL:
            return Node(key, black, RedBlackTree.NULL, RedBlackTree.NULL, right_tree, right_tree.black_height + 1)

        def _left_rotate(node):
            right_child = node.right
            node.right = right_child.left

            if right_child.left != RedBlackTree.NULL:
                right_child.left.parent = node

            right_child.parent = node.parent

            if node.parent != RedBlackTree.NULL:
                if node == node.parent.left:
                    node.parent.left = right_child
                else:
                    node.parent.right = right_child

            right_child.left = node
            node.parent = right_child
            return right_child

        if Node.rank(left_tree) == (Node.rank(right_tree) // 2) * 2:
            return Node(key, red, RedBlackTree.NULL, left_tree, right_tree, left_tree.black_height)
        tree = Node(left_tree.key, left_tree.color, RedBlackTree.NULL, left_tree.left, RedBlackTree._join_right(left_tree.right, key, right_tree), left_tree.black_height)

        if left_tree.color == black and tree.right.color == red and tree.right.right.color == red:
            tree.right.right.color = black
            tree = _left_rotate(tree)
            tree.color = left_tree.color
        return tree

    @staticmethod
    def _join_left(left_tree, key, right_tree):
        if right_tree is None or right_tree == RedBlackTree.NULL:
            return Node(key, black, RedBlackTree.NULL, left_tree, RedBlackTree.NULL, left_tree.black_height + 1)

        def _right_rotate(node):
            left_child = node.left
            node.left = left_child.right

            if left_child.right != RedBlackTree.NULL:
                left_child.right.parent = node

            left_child.parent = node.parent

            if node.parent != RedBlackTree.NULL:
                if node == node.parent.right:
                    node.parent.right = left_child
                else:
                    node.parent.left = left_child

            left_child.right = node
            node.parent = left_child
            return left_child

        if Node.rank(right_tree) == (Node.rank(left_tree) // 2) * 2:
            return Node(key, red, RedBlackTree.NULL, left_tree, right_tree, right_tree.black_height)
        tree = Node(right_tree.key, right_tree.color, RedBlackTree.NULL,
                    RedBlackTree._join_left(left_tree, key, right_tree.left), right_tree.right, right_tree.black_height)

        if right_tree.color == black and tree.left.color == red and tree.left.left.color == red:
            tree.left.left.color = black
            tree = _right_rotate(tree)
            tree.color = right_tree.color
        return tree

    @staticmethod
    def join(left_tree, key, right_tree):
        if Node.rank(left_tree) // 2 > Node.rank(right_tree) // 2:
            tree = RedBlackTree._join_right(left_tree, key, right_tree)
            if tree.color == red and tree.right.color == red:
                tree = Node(tree.key, black, RedBlackTree.NULL, tree.left, tree.right)
        elif Node.rank(left_tree) // 2 < Node.rank(right_tree) // 2:
            tree = RedBlackTree._join_left(left_tree, key, right_tree)
            if tree.color == red and tree.left.color == red:
                tree = Node(tree.key, black, RedBlackTree.NULL, tree.left, tree.right)
        elif left_tree.color == black and right_tree.color == black:
            tree = Node(key, red, RedBlackTree.NULL, left_tree, right_tree)
        else:
            tree = Node(key, black, RedBlackTree.NULL, left_tree, right_tree)

        return tree

    @staticmethod
    def join2(left_tree, right_tree):
        if left_tree.is_nil:
            return right_tree
        left_split, key_split = RedBlackTree.split_last(left_tree)
        return RedBlackTree.join(left_split, key_split, right_tree)

    @staticmethod
    def split(tree, key):
        if tree == RedBlackTree.NULL:
            return RedBlackTree.NULL, False, RedBlackTree.NULL
        if key == tree.key:
            return tree.left, True, tree.right
        elif key < tree.key:
            l_left, b, l_right = RedBlackTree.split(tree.left, key)
            return l_left, b, RedBlackTree.join(l_right, tree.key, tree.right)
        else:
            r_left, b, r_right = RedBlackTree.split(tree.right, key)
            return RedBlackTree.join(tree.left, tree.key, r_left), b, r_right

    @staticmethod
    def split_last(tree):
        if tree.right == RedBlackTree.NULL:
            return tree.left, tree.key
        right_split, key_split = RedBlackTree.split_last(tree.right)
        return RedBlackTree.join(tree.left, tree. key, right_split), key_split

    @staticmethod
    def union(tree_1, tree_2):
        def union_rec(tree1, tree2):
            if tree1 == RedBlackTree.NULL:
                return tree2
            if tree2 == RedBlackTree.NULL:
                return tree1

            left2, tree2_key, right2 = Node.expose(tree2)
            left1, b, right1 = RedBlackTree.split(tree1, tree2_key)

            t_left = union_rec(left1, left2)
            t_right = union_rec(right1, right2)

            return RedBlackTree.join(t_left, tree2_key, t_right)

        union_tree = union_rec(tree_1.root, tree_2.root)
        return RedBlackTree(union_tree)

    @staticmethod
    def intersection(tree_1, tree_2):
        def intersection_rec(tree1, tree2):
            if tree1 == RedBlackTree.NULL or tree2 == RedBlackTree.NULL:
                return RedBlackTree.NULL

            left2, tree2_key, right2 = Node.expose(tree2)
            left1, in_tree1, right1 = RedBlackTree.split(tree1, tree2.key)

            t_left = intersection_rec(left1, left2)
            t_right = intersection_rec(right1, right2)

            if in_tree1:
                return RedBlackTree.join(t_left, tree2_key, t_right)
            return RedBlackTree.join2(t_left, t_right)

        intersection_tree = intersection_rec(tree_1.root, tree_2.root)
        return RedBlackTree(intersection_tree)

    @staticmethod
    def difference(tree_1, tree_2):
        def difference_rec(tree1, tree2):
            if tree1 == RedBlackTree.NULL:
                return RedBlackTree.NULL
            if tree2 == RedBlackTree.NULL:
                return tree1

            left2, tree2_key, right2 = Node.expose(tree2)
            left1, in_tree1, right1 = RedBlackTree.split(tree1, tree2.key)

            t_left = difference_rec(left1, left2)
            t_right = difference_rec(right1, right2)

            return RedBlackTree.join2(t_left, t_right)

        difference_tree = difference_rec(tree_1.root, tree_2.root)
        return RedBlackTree(difference_tree)

    # non static versions of Union, intersction, and difference
    def union2(self, tree_2):
        def union_rec(tree1, tree2):
            if tree1 == RedBlackTree.NULL:
                return tree2
            if tree2 == RedBlackTree.NULL:
                return tree1

            left2, tree2_key, right2 = Node.expose(tree2)
            left1, b, right1 = RedBlackTree.split(tree1, tree2.key)

            t_left = union_rec(left1, left2)
            t_right = union_rec(right1, right2)

            return RedBlackTree.join(t_left, tree2_key, t_right)

        self.root = union_rec(self.root, tree_2.root)

    def intersection2(self, tree):
        def intersection_rec(tree1, tree2):
            if tree1 == RedBlackTree.NULL or tree2 == RedBlackTree.NULL:
                return RedBlackTree.NULL

            left2, tree2_key, right2 = Node.expose(tree2)
            left1, in_tree1, right1 = RedBlackTree.split(tree1, tree2.key)

            t_left = intersection_rec(left1, left2)
            t_right = intersection_rec(right1, right2)

            if in_tree1:
                return RedBlackTree.join(t_left, tree2_key, t_right)
            return RedBlackTree.join2(t_left, t_right)

        self.root = intersection_rec(self.root, tree.root)

    def difference2(self, tree):
        def difference_rec(tree1, tree2):
            if tree1 == RedBlackTree.NULL:
                return RedBlackTree.NULL
            if tree2 == RedBlackTree.NULL:
                return tree1

            left2, tree2_key, right2 = Node.expose(tree2)
            left1, in_tree1, right1 = RedBlackTree.split(tree1, tree2.key)

            t_left = difference_rec(left1, left2)
            t_right = difference_rec(right1, right2)

            return RedBlackTree.join2(t_left, t_right)

        self.root = difference_rec(self.root, tree.root)


def test():
    # Create an instance of RedBlackTree
    rbt1 = RedBlackTree()

    # Insert elements into the red-black tree
    keys = [20, 15, 25, 10, 5, 1]
    for key in keys:
        rbt1.insert(key)

    # Print the red-black tree
    print("Red-Black Tree 1:")
    print(rbt1)

    # Perform union, intersection, and set difference operations
    rbt2 = RedBlackTree()
    keys2 = [30, 25, 40, 35]
    for key in keys2:
        rbt2.insert(key)

    print("\nRed-Black Tree 2:")
    print(rbt2)

    print("\nUnion of Tree 1 and Tree 2:")
    union_tree = RedBlackTree.union(rbt1, rbt2)
    print(union_tree)
    #
    print("\nIntersection of Tree 1 and Tree 2:")
    intersection_tree = RedBlackTree.intersection(rbt1, rbt2)
    print(intersection_tree)

    print("\nSet Difference of Tree 1 and Tree 2:")
    difference_tree = RedBlackTree.difference(rbt1, rbt2)
    print(difference_tree)

    print("group 1: *********************************************")
    print(rbt1.values())
    print("group 2: *********************************************")
    print(rbt2.values())
    print("union:*********************************************")
    print(union_tree.values())
    print(len(union_tree.values()))
    # print("intersection:*********************************************")
    # print(intersection_tree.inorder())
    # print("difference: *********************************************")
    # print(difference_tree.inorder())

def print_1000():
    trees = []
    with open("data_struct(1000x1000).txt", "r") as file:
        for line in file:
            numbers = map(float, line.split(','))
            tree = RedBlackTree()
            for number in numbers:
                tree.insert(number)
            trees.append(tree)
            # print(f"tree size: ({len(tree.values())})")


    if trees:
        union_tree = trees[0]
        print(union_tree)
        for i in range(1, len(trees)):
            # print(f"<BEFORE: union tree size: ({len(union_tree.values())}) | tree to add size: ({len(trees[i].values())})")
            # union_tree = RedBlackTree.union(union_tree, trees[i])
            union_tree.union2(trees[i])
            # print(f">AFTER: union tree size: ({len(union_tree.values())})")
        print(len(union_tree.values()))

        for key in union_tree.values():
            union_tree.delete(key)

        print("Union tree after deletions:", len(union_tree.values()))

# print_1000()
test()
