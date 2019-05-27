package org.asarkar.codinginterview.bintree;

import java.util.*;

public class BinTrees {
    private BinTrees() {
    }

    /*
     * Do an inorder traversal using O(1) memory.
     *
     * ANSWER: We implement Morris inorder traversal. https://www.youtube.com/watch?v=wGXB9OWhPTg
     *
     * Since we don't want to use a stack (O(h) space), explicitly, or via recursion, we temporarily modify the
     * left subtree by adding a link back to its inorder successor from every leaf node. In other words,
     * every node's inorder predecessor is modified to have its right child point to the node. At each node, we check
     * if its inorder predecessor has a right child that points to the node; if so, we have already visited the left
     * subtree, and can emit the value of the node (also remove the temporary link). If not, we establish the link,
     * and visit the left subtree.
     */
    static <T> List<T> morrisInorder(Node<T> root) {
        List<T> inorder = new ArrayList<>();

        Node<T> current = root;
        while (current != null) {
            Node<T> pred = inorderPredecessor(current);

            // no left subtree or already visited
            if (pred == null || pred.right == current) {
                // remove parent pointer
                if (pred != null) {
                    pred.right = null;
                }
                inorder.add(current.datum);
                current = current.right;
            }
            // create parent pointer, and then visit left subtree
            else {
                pred.right = current;
                current = current.left;
            }
        }

        return inorder;
    }

    // rightmost node in the left subtree
    private static <T> Node<T> inorderPredecessor(Node<T> root) {
        if (root.left == null) {
            return null;
        }

        Node<T> current = root.left;

        while (current.right != null && current.right != root) {
            current = current.right;
        }

        return current;
    }

    /*
     * Create a BST from an array
     *
     * ANSWER: Pick the middle element, and recursively build the left and right subtrees.
     */
    static <T extends Comparable<T>> Node<T> buildBST(T[] data) {
        Arrays.sort(data);
        return buildBST(data, 0, data.length - 1);
    }

    private static <T extends Comparable<T>> Node<T> buildBST(T[] data, int start, int end) {
        if (start > end) {
            return null;
        }
        int middle = start + ((end - start) >> 1);

        Node<T> left = buildBST(data, start, middle - 1);
        Node<T> right = buildBST(data, middle + 1, end);

        return new Node<>(left, right, data[middle]);
    }

    static <T> List<T> levelOrder(Node<T> root) {
        Queue<Node<T>> queue = new LinkedList<>();
        queue.offer(root);
        List<T> list = new ArrayList<>();

        while (!queue.isEmpty()) {
            Node<T> parent = queue.poll();
            list.add(parent.datum);
            if (parent.left != null) {
                queue.offer(parent.left);
            }
            if (parent.right != null) {
                queue.offer(parent.right);
            }
        }

        return list;
    }

    /*
     * Invert a binary tree.
     *
     * For example, given the following tree:
     *
     *       a
     *     +  +
     *    b     c
     * +----+  +
     * d    e f
     *
     * should become:
     *
     *      a
     *   ++ +--+
     *  c       b
     *   +    +   +
     *   f   e     d

     */
    static <T> Node<T> invertTree(Node<T> root) {
        if (root == null) {
            return root;
        }

        Node<T> left = invertTree(root.left);
        root.left = invertTree(root.right);
        root.right = left;

        return root;
    }
}
