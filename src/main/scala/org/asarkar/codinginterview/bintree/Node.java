package org.asarkar.codinginterview.bintree;

public class Node<T> {
    Node<T> left;
    Node<T> right;
    T datum;

    Node(Node<T> left, Node<T> right, T datum) {
        this.left = left;
        this.right = right;
        this.datum = datum;
    }

    Node(T[] data) {
        Node<T> root = buildTree(0, data);
        this.left = root.left;
        this.right = root.right;
        this.datum = root.datum;
    }

    public Node<T> getLeft() {
        return left;
    }

    public Node<T> getRight() {
        return right;
    }

    public T getDatum() {
        return datum;
    }

    private Node<T> buildTree(int i, T[] data) {
        // In a complete binary tree, half of the nodes are leaves
        if (i > data.length / 2 && i < data.length) {
            return data[i] == null ? null : new Node<>(null, null, data[i]);
        } else if (i >= data.length || data[i] == null) {
            return null;
        }

        int left = 2 * i + 1;
        int right = left + 1;

        Node<T> leftChild = buildTree(left, data);
        Node<T> rightChild = buildTree(right, data);

        return new Node<>(leftChild, rightChild, data[i]);
    }
}
