package org.asarkar.codinginterview.lists;

public class Lists {
    /*
     * Given a singly-linked list, write a method isListPalindrome to determine if the list is a palindrome.
     * A palindrome is a sequence that reads the same backward as forward.
     *
     * ANSWER: We find the middle node by the slow-fast pointers method. We then reverse the sublist starting with the
     * middle element. Lastly, we compare elements from both ends.
     *
     * Time complexity: O(n).
     */
    public static <T> boolean isPalindrome(Node<T> head) {
        if (head == null || head.datum == null) {
            return false;
        }
        Node<T> fast = head;
        Node<T> slow = head;

        while (fast != null && fast.next != null) {
            slow = slow.next;
            fast = fast.next.next;
        }

        Node<T> prev = null;
        Node<T> cur = slow;
        Node<T> next;

        while (cur != null) {
            // save a reference to the next node, because we are about to reverse the next pointer from the current
            // node to point to the previous one. this sets the next pointer of the middle node to null, exactly what
            // we want
            next = cur.next;
            cur.next = prev;
            prev = cur;
            cur = next;
        }

        cur = head;
        next = prev;

        while (cur != next && next != null) {
            if (cur.datum == next.datum) {
                cur = cur.next;
                next = next.next;
            } else {
                return false;
            }
        }
        return true;
    }
}
