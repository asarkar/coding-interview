package org.asarkar.codinginterview.lists;

import java.util.Collection;
import java.util.Iterator;

public class Node<T> {
    Node<T> next;
    T datum;

    public Node(Collection<T> data) {
        if (!data.isEmpty()) {
            Iterator<T> it = data.iterator();

            this.datum = it.next();
            Node<T> cur = this;

            while (it.hasNext()) {
                cur.next = new Node<>(it.next());
                cur = cur.next;
            }
        }
    }

    public Node(T datum) {
        this(null, datum);
    }

    public Node(Node<T> next, T datum) {
        this.next = next;
        this.datum = datum;
    }

    public Node<T> getNext() {
        return next;
    }

    public T getDatum() {
        return datum;
    }
}
