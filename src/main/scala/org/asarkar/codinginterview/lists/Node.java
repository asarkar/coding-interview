package org.asarkar.codinginterview.lists;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

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

    Node(T datum) {
        this(null, datum);
    }

    Node(Node<T> next, T datum) {
        this.next = next;
        this.datum = datum;
    }

    public List<T> toList() {
        Node<T> cur = this;
        List<T> list = new ArrayList<>();

        while (cur != null) {
            list.add(cur.datum);
            cur = cur.next;
        }

        return list;
    }

    @Override
    public String toString() {
        return datum == null ? null : datum.toString();
    }
}
