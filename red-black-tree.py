class Node:
    def __init__(self, key, color, left=None, right=None, black_height=None):
        self.key = key
        self.color = color  # True for black, False for red
        self.left = left
        self.right = right
        if black_height is None:
            if color:
                black_height = 1
            else:
                black_height = 0
        self.black_height = black_height

    def print_subtree(self, node=None):
        def print_helper(node=None, indent="", last=True):
            if node is not None:
                print(indent, end="")
                if last:
                    print("R----", end="")
                    indent += "     "
                else:
                    print("L----", end="")
                    indent += "|    "
                color = "RED" if not node.color else "BLACK"
                print(f"{node.key}({color})")
                print_helper(node.left, indent, False)
                print_helper(node.right, indent, True)

        if node is None:
            print_helper(self)
        else:
            print_helper(node)


class RedBlackTree:
    def __init__(self, root=None):
        self.root = root

    @staticmethod
    def is_red(node):
        return node is not None and not node.color

    @staticmethod
    def is_black(node):
        return node is None or node.color

    @staticmethod
    def black_height(node):
        return 0 if node is None else node.black_height

    @staticmethod
    def rotate_left(node):
        right_tree = node.right
        node.right = right_tree.left

        right_tree.left = node
        right_tree.color = node.color
        node.color = False
        right_tree.black_height = node.black_height
        node.black_height -= 1
        return right_tree

    @staticmethod
    def rotate_right(tree):
        new_tree = tree.left
        tree.left = new_tree.right
        new_tree.right = tree
        new_tree.color = tree.color
        tree.color = False
        new_tree.black_height = tree.black_height
        tree.black_height -= 1
        return new_tree

    @staticmethod
    def flip_colors(tree):
        tree.color = not tree.color
        tree.left.color = not tree.left.color
        tree.right.color = not tree.right.color

    @staticmethod
    def fix_up(tree):
        if RedBlackTree.is_red(tree.right) and not RedBlackTree.is_red(tree.left):
            tree = RedBlackTree.rotate_left(tree)
        if RedBlackTree.is_red(tree.left) and RedBlackTree.is_red(tree.left.left):
            tree = RedBlackTree.rotate_right(tree)
        if RedBlackTree.is_red(tree.left) and RedBlackTree.is_red(tree.right):
            RedBlackTree.flip_colors(tree)

        tree.black_height = RedBlackTree.black_height(tree.left) + (1 if RedBlackTree.is_black(tree) else 0)
        return tree

    def search(self, key):
        def search_helper(node, key):
            if node is None:
                return None

            if node.key == key:
                return node

            if key < node.key:
                return search_helper(node.left, key)
            return search_helper(node.right, key)

        search_result = search_helper(self.root, key)
        return search_result

    def insert(self, key):
        def insert_rec(tree, key):
            if tree is None:
                return Node(key, False)

            if key < tree.key:
                tree.left = insert_rec(tree.left, key)
            elif key > tree.key:
                tree.right = insert_rec(tree.right, key)
            else:
                return tree  # Key already in the tree

            return self.fix_up(tree)

        self.root = insert_rec(self.root, key)
        if self.root:
            self.root.color = True

    def delete(self, key):
        def delete_rec(tree, key):
            if tree is None:
                return None

            if key < tree.key:
                tree.left = delete_rec(tree.left, key)
            elif key > tree.key:
                tree.right = delete_rec(tree.right, key)
            else:
                if tree.left is None:
                    return tree.right
                if tree.right is None:
                    return tree.left

                left, last_key = self.split_last(tree.left)
                tree.key = last_key
                tree.left = left

            return self.fix_up(tree)

        self.root = delete_rec(self.root, key)
        if self.root:
            self.root.color = True

    @staticmethod
    def join_right(left_tree, key, right_tree):
        # if left_tree is None:
        #     return right_tree
        # if right_tree is None:
        #     return left_tree

        if RedBlackTree.black_height(left_tree) < RedBlackTree.black_height(right_tree):
            right_tree.left = RedBlackTree.join_right(left_tree, key, right_tree.left)
            return RedBlackTree.fix_up(right_tree)
        elif RedBlackTree.black_height(left_tree) > RedBlackTree.black_height(right_tree):
            left_tree.right = RedBlackTree.join_right(left_tree.right, key, right_tree)
            return RedBlackTree.fix_up(left_tree)
        else:
            return Node(key, False, left_tree, right_tree, RedBlackTree.black_height(left_tree) + 1)

    @staticmethod
    def join_left(left_tree, key, right_tree):

        # if right_tree is None:
        #     return left_tree
        # if left_tree is None:
        #     return right_tree

        if RedBlackTree.black_height(left_tree) < RedBlackTree.black_height(right_tree):
            right_tree.left = RedBlackTree.join_left(left_tree, key, right_tree.left)
            return RedBlackTree.fix_up(right_tree)
        elif RedBlackTree.black_height(left_tree) > RedBlackTree.black_height(right_tree):
            left_tree.right = RedBlackTree.join_left(left_tree.right, key, right_tree)
            return RedBlackTree.fix_up(left_tree)
        else:
            return Node(key, False, left_tree, right_tree, RedBlackTree.black_height(left_tree) + 1)

    @staticmethod
    def join(left_tree, key, right_tree):
        if RedBlackTree.black_height(left_tree) <= RedBlackTree.black_height(right_tree):
            return RedBlackTree.join_right(left_tree, key, right_tree)
        else:
            return RedBlackTree.join_left(left_tree, key, right_tree)

    @staticmethod
    def join_with_key(left_tree, key, right_tree):
        if key is None:
            if left_tree is None:
                return right_tree
            if right_tree is None:
                return left_tree
            return RedBlackTree.join(left_tree, right_tree.key, right_tree.right)
        else:
            return RedBlackTree.join(left_tree, key, right_tree)

    @staticmethod
    def split(tree, key):
        if tree is None:
            return None, False, None

        if key < tree.key:
            left, found, right = RedBlackTree.split(tree.left, key)
            return left, found, RedBlackTree.join(right, tree.key, tree.right)
        elif key > tree.key:
            left, found, right = RedBlackTree.split(tree.right, key)
            return RedBlackTree.join(tree.left, tree.key, left), found, right
        else:
            return tree.left, True, tree.right

    @staticmethod
    def split_last(tree):
        if tree.right is None:
            return tree.left, tree.key
        else:
            left, last_key = RedBlackTree.split_last(tree.right)
            return RedBlackTree.join(tree.left, tree.key, left), last_key

    @staticmethod
    def union(tree1, tree2):
        def union_helper(tree1, tree2):
            if tree1 is None:
                return tree2
            if tree2 is None:
                return tree1

            left1, in_tree1, right1 = RedBlackTree.split(tree1, tree2.key)
            left2, in_tree2, right2 = RedBlackTree.split(tree2, tree2.key)

            merged_left = union_helper(left1, left2)
            merged_right = union_helper(right1, right2)
            return RedBlackTree.join_with_key(merged_left, tree2.key if in_tree1 or in_tree2 else None, merged_right)
        return RedBlackTree(union_helper(tree1.root, tree2.root))

    @staticmethod
    def intersection(tree1, tree2):
        def intersection_helper(tree1, tree2):
            if tree1 is None or tree2 is None:
                return None

            left1, in_tree1, right1 = RedBlackTree.split(tree1, tree2.key)
            left2, in_tree2, right2 = RedBlackTree.split(tree2, tree2.key)

            if in_tree1 and in_tree2:
                intersected_left = intersection_helper(left1, left2)
                intersected_right = intersection_helper(right1, right2)
                return RedBlackTree.join_with_key(intersected_left, tree2.key, intersected_right)
            else:
                intersected_left = intersection_helper(left1, left2)
                intersected_right = intersection_helper(right1, right2)
                return RedBlackTree.join_with_key(intersected_left, None, intersected_right)
        return RedBlackTree(intersection_helper(tree1.root, tree2.root))

    @staticmethod
    def set_difference(tree1, tree2):
        def set_difference_helper(tree1, tree2):
            if tree1 is None:
                return None
            if tree2 is None:
                return tree1

            left1, in_tree1, right1 = RedBlackTree.split(tree1, tree2.key)
            left2, in_tree2, right2 = RedBlackTree.split(tree2, tree2.key)

            if in_tree2:
                diff_left = set_difference_helper(left1, left2)
                diff_right = set_difference_helper(right1, right2)
                return RedBlackTree.join_with_key(diff_left, None, diff_right)
            else:
                diff_left = set_difference_helper(left1, left2)
                diff_right = set_difference_helper(right1, right2)
                return RedBlackTree.join_with_key(diff_left, tree1.key, diff_right)
        return RedBlackTree(set_difference_helper(tree1.root, tree2.root))


    def print_tree(self):
        self.root.print_subtree()

    def values(self):
        def inorder_helper(node, res):
            if node is not None:
                inorder_helper(node.left, res)
                res.append(node.key)
                inorder_helper(node.right, res)
        res = []
        inorder_helper(self.root, res)
        return res



def test():
    # Create an instance of RedBlackTree
    rbt = RedBlackTree()

    # Insert elements into the red-black tree
    keys = [20, 15, 25, 10, 5, 1]
    for key in keys:
        rbt.insert(key)
        # rbt.print_tree()
        # print("********************************************",key)

    # Print the red-black tree
    print("Red-Black Tree:")
    rbt.print_tree()

    # Perform union, intersection, and set difference operations
    rbt2 = RedBlackTree()
    keys2 = [30, 25, 40, 35]
    for key in keys2:
        rbt2.insert(key)

    print("\nRed-Black Tree 2:")
    rbt2.print_tree()

    print("\nUnion of Tree 1 and Tree 2:")
    union_tree = RedBlackTree.union(rbt, rbt2)
    # rbt.print_tree(union_tree)
    union_tree.print_tree()

    print("\nIntersection of Tree 1 and Tree 2:")
    intersection_tree = RedBlackTree.intersection(rbt, rbt2)
    intersection_tree.print_tree()

    print("\nSet Difference of Tree 1 and Tree 2:")
    difference_tree = RedBlackTree.set_difference(rbt, rbt2)
    difference_tree.print_tree()

    print(union_tree.values())


def print_1000():
    trees = []
    with open("data_struct(1000x1000).txt", "r") as file:
        for line in file:
            numbers = map(float, line.split(','))
            tree = RedBlackTree()
            for number in numbers:
                tree.insert(number)
            trees.append(tree)


    if trees:
        union_tree = trees[0]
        for i in range(1, len(trees)):
            union_tree = RedBlackTree.union(union_tree,trees[i])

        union_tree.print_tree()
        print(union_tree.values())


    # for key in union_tree.values():
    #     union_tree.delete(key)
    #
    # print("Union tree after deletions:", union_tree.values())


# print_1000()
test()