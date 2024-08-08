from collections import deque


red = False
black = True

class Node:
    def __init__(self, key, color=red):
        self.key = key
        self.color = color
        self.parent = None
        self.left = None
        self.right = None
        self.black_height = 0

    def __str__(self):
        return f"{self.key}({self.print_color()})"

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

class RedBlackTree:
    NULL = Node(0, black)
    
    def __init__(self):
        self.root = self.NULL

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

    def _search_helper(self, node, key):
        if node == self.NULL:
            return self.NULL

        if node.key == key:
            return node

        if key < node.key:
            return self._search_helper(node.left, key)
        return self._search_helper(node.right, key)

    def _right_rotate(self, node):
        left_child = node.left
        node.left = left_child.right

        if left_child.right != self.NULL:
            left_child.right.parent = node

        left_child.parent = node.parent

        if node.parent is self.NULL:
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

        if node.parent is self.NULL:
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
                print("duplicates not supported")
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

    def inorder(self):
        def inorder_helper(node, result):
            if node != self.NULL:
                inorder_helper(node.left, result)
                result.append(node.key)
                inorder_helper(node.right, result)
        res = []
        inorder_helper(self.root, res)
        return res


class Group:
    def __init__(self):
        self.tree = RedBlackTree()
        self.size = 0

    def __str__(self):
        print(self.tree)
        return ""

    def insert(self, value):
        if self.tree.insert(value):
            self.size += 1
            print(f"Successfully Inserted {value}")
        else:
            print(f"Failed To Insert {value}")

    def remove(self, value):
        if self.tree.remove(value):
            self.size -= 1
            print(f"Successfully Removed {value}")
        else:
            print(f"Failed To Remove {value}")

    def contains(self, value):
        return self.tree.search(value) is not None

    def values(self):
        return self.tree.inorder()



group = Group()
group.insert(4)
print(group)
group.insert(12)
print(group)
group.insert(9)
print(group)
group.insert(2)
print(group)
group.insert(6)
print(group)
print(group)
group.insert(45)
print(group)
group.insert(8)
print(group)
group.insert(15)
print(group)
group.insert(5)
print(group)
group.insert(10)
print(group)
group.insert(16)
print(group)
group.insert(7)
print(group)


print(group.values())


print(group)
# result = group.contains(10)
# print(result)
# group.remove(10)
# print(group)
# result = group.contains(10)
# print(result)
#
#
# group.remove(6)
# print(group)
#
#
# group.remove(9)
# print(group)
#
#
