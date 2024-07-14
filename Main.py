# Full implementation of a Red-Black Tree in python for Scientific Programming

class RedBlackTreeNode:
    def __init__(self, key, color="red"):
        self.key = key
        self.color = color
        self.left = None
        self.right = None
        self.parent = None


class RedBlackTree:
    def __init__(self):
        self.TNULL = RedBlackTreeNode(0)
        self.TNULL.color = "black"
        self.root = self.TNULL

    def _search_tree_helper(self, node, key):
        if node == self.TNULL or key == node.key:
            return node

        if key < node.key:
            return self._search_tree_helper(node.left, key)
        return self._search_tree_helper(node.right, key)

    def search(self, key):
        print("search")
        return self._search_tree_helper(self.root, key) != self.TNULL

    def _balance_insert(self, k):
        while k.parent.color == "red":
            if k.parent == k.parent.parent.right:
                u = k.parent.parent.left
                if u.color == "red":
                    u.color = "black"
                    k.parent.color = "black"
                    k.parent.parent.color = "red"
                    k = k.parent.parent
                else:
                    if k == k.parent.left:
                        k = k.parent
                        self._right_rotate(k)
                    k.parent.color = "black"
                    k.parent.parent.color = "red"
                    self._left_rotate(k.parent.parent)
            else:
                u = k.parent.parent.right
                if u.color == "red":
                    u.color = "black"
                    k.parent.color = "black"
                    k.parent.parent.color = "red"
                    k = k.parent.parent
                else:
                    if k == k.parent.right:
                        k = k.parent
                        self._left_rotate(k)
                    k.parent.color = "black"
                    k.parent.parent.color = "red"
                    self._right_rotate(k.parent.parent)
            if k == self.root:
                break
        self.root.color = "black"

    def _balance_delete(self, x):
        while x != self.root and x.color == "black":
            if x == x.parent.left:
                s = x.parent.right
                if s.color == "red":
                    s.color = "black"
                    x.parent.color = "red"
                    self._left_rotate(x.parent)
                    s = x.parent.right

                if s.left.color == "black" and s.right.color == "black":
                    s.color = "red"
                    x = x.parent
                else:
                    if s.right.color == "black":
                        s.left.color = "black"
                        s.color = "red"
                        self._right_rotate(s)
                        s = x.parent.right

                    s.color = x.parent.color
                    x.parent.color = "black"
                    s.right.color = "black"
                    self._left_rotate(x.parent)
                    x = self.root
            else:
                s = x.parent.left
                if s.color == "red":
                    s.color = "black"
                    x.parent.color = "red"
                    self._right_rotate(x.parent)
                    s = x.parent.left

                if s.left.color == "black" and s.left.color == "black":
                    s.color = "red"
                    x = x.parent
                else:
                    if s.left.color == "black":
                        s.right.color = "black"
                        s.color = "red"
                        self._left_rotate(s)
                        s = x.parent.left

                    s.color = x.parent.color
                    x.parent.color = "black"
                    s.left.color = "black"
                    self._right_rotate(x.parent)
                    x = self.root
        x.color = "black"

    def _rb_transplant(self, u, v):
        if u.parent == None:
            self.root = v
        elif u == u.parent.left:
            u.parent.left = v
        else:
            u.parent.right = v
        v.parent = u.parent

    def _delete_node_helper(self, node, key):
        z = self.TNULL
        while node != self.TNULL:
            if node.key == key:
                z = node

            if node.key <= key:
                node = node.right
            else:
                node = node.left

        if z == self.TNULL:
            print("Couldn't find key in the tree")
            return

        y = z
        y_original_color = y.color
        if z.left == self.TNULL:
            x = z.right
            self._rb_transplant(z, z.right)
        elif z.right == self.TNULL:
            x = z.left
            self._rb_transplant(z, z.left)
        else:
            y = self._minimum(z.right)
            y_original_color = y.color
            x = y.right
            if y.parent == z:
                x.parent = y
            else:
                self._rb_transplant(y, y.right)
                y.right = z.right
                y.right.parent = y

            self._rb_transplant(z, y)
            y.left = z.left
            y.left.parent = y
            y.color = z.color
        if y_original_color == "black":
            self._balance_delete(x)

    def _left_rotate(self, x):
        y = x.right
        x.right = y.left
        if y.left != self.TNULL:
            y.left.parent = x

        y.parent = x.parent
        if x.parent == None:
            self.root = y
        elif x == x.parent.left:
            x.parent.left = y
        else:
            x.parent.right = y
        y.left = x
        x.parent = y

    def _right_rotate(self, x):
        y = x.left
        x.left = y.right
        if y.right != self.TNULL:
            y.right.parent = x

        y.parent = x.parent
        if x.parent == None:
            self.root = y
        elif x == x.parent.right:
            x.parent.right = y
        else:
            x.parent.left = y
        y.right = x
        x.parent = y

    def _minimum(self, node):
        while node.left != self.TNULL:
            node = node.left
        return node

    def insert(self, key):
        node = RedBlackTreeNode(key)
        node.parent = None
        node.key = key
        node.left = self.TNULL
        node.right = self.TNULL
        node.color = "red"

        y = None
        x = self.root

        print("insert", key)
        while x != self.TNULL:
            y = x
            if node.key < x.key:
                x = x.left
            else:
                x = x.right

        node.parent = y
        if y == None:
            self.root = node
        elif node.key < y.key:
            y.left = node
        else:
            y.right = node

        if node.parent == None:
            node.color = "black"
            return

        if node.parent.parent == None:
            return

        self._balance_insert(node)

    def delete(self, key):
        self._delete_node_helper(self.root, key)

    def _inorder_helper(self, node, res):
        if node != self.TNULL:
            self._inorder_helper(node.left, res)
            res.append(node.key)
            self._inorder_helper(node.right, res)

    def inorder(self):
        res = []
        self._inorder_helper(self.root, res)
        return res

    def union(self, other_tree):
        result = RedBlackTree()
        for key in self.inorder():
            result.insert(key)
        for key in other_tree.inorder():
            if not result.search(key):
                result.insert(key)
        return result

    def intersection(self, other_tree):
        result = RedBlackTree()
        for key in self.inorder():
            if other_tree.search(key):
                result.insert(key)
        return result

    def difference(self, other_tree):
        result = RedBlackTree()
        for key in self.inorder():
            if not other_tree.search(key):
                result.insert(key)
        return result

# Usage example
if __name__ == "__main__":
    # Read the file and create Red-Black Trees from each row
    trees = []
    with open("data_struct(1000x1000).txt", "r") as file:
        for line in file:
            numbers = map(float, line.split(','))
            tree = RedBlackTree()
            for number in numbers:
                tree.insert(number)
            trees.append(tree)

    # Perform union of all trees
    if trees:
        union_tree = trees[0]
        for i in range(1, len(trees)):
            union_tree = union_tree.union(trees[i])

    # Delete all elements from the union tree
    for key in union_tree.inorder():
        union_tree.delete(key)

    print("Union tree after deletions:", union_tree.inorder())
