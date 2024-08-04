
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
        self.size = 0

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

        queue = deque([(self.root, 0)])
        current_level = 0
        level_nodes = []

        while queue:
            node, level = queue.popleft()
            if level > current_level:
                self.print_level(level_nodes, current_level)
                level_nodes = []
                current_level = level

            color = "R" if node.color == red else "B"
            level_nodes.append((node.key, color))

            if node.left and node.left != self.NULL:
                queue.append((node.left, level + 1))
            if node.right and node.right != self.NULL:
                queue.append((node.right, level + 1))

        if level_nodes:
            self.print_level(level_nodes, current_level)

    def print_level(self, nodes, level):
        indent = " " * (2 ** (4 - level))
        print(indent + "   ".join([f"{key}:{color}" for key, color in nodes]))
        print(indent)

    def search(self, key):
        result = self.search_helper(self.root, key)
        if result == self.NULL:
            return None
        return result

    def search_helper(self, node, key):
        if node == self.NULL:
            return self.NULL

        if node.key == key:
            return node

        if key < node.key:
            return self.search_helper(node.left, key)
        return self.search_helper(node.right, key)

    def right_rotate(self, node):
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

    def left_rotate(self, node):
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
            return


        current = self.root
        parent = self.NULL

        while current != self.NULL:
            parent = current
            if new_node.key < current.key:
                current = current.left
            elif new_node.key > current.key:
                current = current.right
            else:
                print("duplicate keys not supported")
                return

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
                        self.left_rotate(node_to_balance)
                    node_to_balance.parent.color = black
                    node_to_balance.parent.parent.color = red
                    self.right_rotate(node_to_balance.parent.parent)
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
                        self.right_rotate(node_to_balance)
                    node_to_balance.parent.color = black
                    node_to_balance.parent.parent.color = red
                    self.left_rotate(node_to_balance.parent.parent)

        self.root.color = black


tree = RedBlackTree()
tree.insert(4)
tree.insert(12)
tree.insert(9)
tree.insert(2)
tree.insert(6)
tree.insert(45)
tree.insert(8)
tree.insert(15)
tree.insert(5)
tree.insert(10)
tree.insert(16)
tree.insert(7)
tree.insert(0)

print(tree)
result = tree.search(0)
print(result)
