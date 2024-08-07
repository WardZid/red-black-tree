from collections import deque


red = True
black = False

class Node:
    def __init__(self, key, color=red):
        self.key = key
        self.color = color
        self.parent = None
        self.left = None
        self.right = None

    def __str__(self):
        return f"{self.key}({self.print_color()})"

    def print_color(self):
        if self.color == red:
            return "red"
        return "black"

class RedBlackTree:
    def __init__(self):
        self.NULL = Node(0, black)
        self.root = self.NULL

    def __str__(self):
        self.print_bfs()
        return ""

    def print_bfs(self):
        if self.root == self.NULL:
            print("Tree is empty")
            return

        print("***********************************************START")
        queue = deque([(self.root, 0)])
        current_level = 0
        level_nodes = []

        while queue:
            node, level = queue.popleft()
            if level > current_level:
                self._print_level(level_nodes, current_level)
                level_nodes = []
                current_level = level

            color = "R" if node.color == red else "B"
            level_nodes.append((node.key, color))

            if node.left and node.left != self.NULL:
                queue.append((node.left, level + 1))
            if node.right and node.right != self.NULL:
                queue.append((node.right, level + 1))

        if level_nodes:
            self._print_level(level_nodes, current_level)

        print("***********************************************END")

    @staticmethod
    def _print_level(nodes, level):
        indent = " " * (2 ** (4 - level))
        print(indent + "   ".join([f"{key}:{color}" for key, color in nodes]))
        print(indent)

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
                    node_to_balance = node_to_balance.parent.parent #grandparent
                else:
                    is_left_triangle = (node_to_balance == node_to_balance.parent.right)
                    if is_left_triangle:
                        node_to_balance = node_to_balance.parent
                        self._left_rotate(node_to_balance)
                    node_to_balance.parent.color = black
                    node_to_balance.parent.parent.color = red
                    self._right_rotate(node_to_balance.parent.parent)
            else:
                if left_uncle.color == red:
                    node_to_balance.parent.color = black
                    left_uncle.color = black
                    node_to_balance.parent.parent.color = red  # grandparent.color = red
                    node_to_balance = node_to_balance.parent.parent
                else:
                    is_right_triangle = (node_to_balance == node_to_balance.parent.left)
                    if is_right_triangle:
                        node_to_balance = node_to_balance.parent
                        self._right_rotate(node_to_balance)
                    node_to_balance.parent.color = black
                    node_to_balance.parent.parent.color = red
                    self._left_rotate(node_to_balance.parent.parent)

        self.root.color = black
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
                        self._left_rotate(node_to_fix.parent)
                        sibling = node_to_fix.parent.right

                    if sibling.left.color == black and sibling.right.color == black:
                        sibling.color = red
                        node_to_fix = node_to_fix.parent
                    else:
                        if sibling.right.color == black:
                            sibling.left.color = black
                            sibling.color = red
                            self._right_rotate(sibling)
                            sibling = node_to_fix.parent.right
                        sibling.color = node_to_fix.parent.color
                        node_to_fix.parent.color = black
                        sibling.right.color = black
                        self._left_rotate(node_to_fix.parent)
                        node_to_fix = self.root
                else:
                    sibling = node_to_fix.parent.left

                    if sibling.color == red:
                        sibling.color = black
                        node_to_fix.parent.color = red
                        self._right_rotate(node_to_fix.parent)
                        sibling = node_to_fix.parent.left

                    if sibling.left.color == black and sibling.right.color == black:
                        sibling.color = red
                        node_to_fix = node_to_fix.parent
                    else:
                        if sibling.left.color == black:
                            sibling.right.color = black
                            sibling.color = red
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
        res = []
        self._inorder_helper(self.root, res)
        return res

    def _inorder_helper(self, node, res):
        if node != self.NULL:
            self._inorder_helper(node.left, res)
            res.append(node.key)
            self._inorder_helper(node.right, res)


class Group:
    def __init__(self):
        self.tree = RedBlackTree()
        self.size = 0

    def __str__(self):
        self.tree.print_bfs()
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
group.insert(12)
group.insert(9)
group.insert(2)
group.insert(6)
group.insert(45)
group.insert(8)
group.insert(15)
group.insert(5)
group.insert(10)
group.insert(16)
group.insert(7)


print(group.values())


print(group)
result = group.search(10)
print(result)


group.remove(10)
print(group)


group.remove(6)
print(group)


group.remove(9)
print(group)


