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

    /*
     * You are given two non-empty linked lists representing two non-negative integers. The digits are stored in
     * reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.
     * You may assume the two numbers do not contain any leading zero, except the number 0 itself.
     *
     * ANSWER: Grade school addition with digits and carry over. Shorter numbers are assumed to have leading zeros.
     */
    public static Node<Integer> addTwoNumbers(Node<Integer> l1, Node<Integer> l2) {
        Node<Integer> sum = null;
        Node<Integer> head = null;
        int carry = 0;
        Node<Integer> n1 = l1;
        Node<Integer> n2 = l2;

        while (n1 != null || n2 != null) {
            int i = 0;
            if (n1 != null) {
                i = n1.datum;
                n1 = n1.next;
            }
            int j = 0;
            if (n2 != null) {
                j = n2.datum;
                n2 = n2.next;
            }

            int s = i + j + carry;
            carry = s / 10;
            s = s % 10;

            Node<Integer> n = new Node<>(s);
            if (sum == null) {
                sum = n;
                head = sum;
            } else {
                sum.next = n;
                sum = sum.next;
            }
        }

        if (carry > 0) {
            sum.next = new Node<>(carry);
        }

        return head;
    }

    /*
     * Rearrange a linked list of integers to alternating high-low. In other words, rearrange it such that every
     * second node is greater than its left and right neighbors.
     *
     * ANSWER: Observe that nodes at odd positions should be smaller than nodes at even positions. Thus, we keep track
     * of the node position, and when a node at odd position (1st, 3rd, 5th, and so on) is smaller than the one on its
     * right, we swap them. If a node at the even position (2nd, 4th, 6th, and so on) is is smaller than the one on its
     * right, we swap them too. Another way to see it is that we slide a window of size 2 by 1 position each time, and
     * if the left node in that window is smaller than the right one, we swap them.
     *
     * This gives is a correct algorithm; all that left is pointer manipulation.
     */
    public static Node<Integer> loHi(Node<Integer> head) {
        Node<Integer> prev = null;
        Node<Integer> cur = head;
        boolean even = false;
        Node<Integer> newHead = head;

        while (cur != null) {
            Node<Integer> next = cur.next;

            if (next != null && ((even && cur.datum < next.datum) || (!even && cur.datum > next.datum))) {
                Node<Integer> nextNext = next.next;
                if (prev == null) {
                    newHead = next;
                } else {
                    prev.next = next;
                }
                next.next = cur;
                cur.next = nextNext;

                prev = next;
            } else {
                prev = cur;
                cur = next;
            }
            even = !even;
        }

        return newHead;
    }

    /*
     * Write a program to find the node at which the intersection of two singly linked lists begins.
     * For example:
     *      4+->1
     *          +
     *          +->8+->4+->5
     *          ^
     * 5+->0+->1+
     *
     * The above lists intersect at 8.
     *
     * ANSWER: We first find the length of both lists. Observe that since the length of the common portion is the same,
     * the difference in length, if any, comes from the portions before the intersection. Thus, if we advance a pointer
     * along the longer list by the difference, and then onwards, advance pointers along both lists at tandem, they
     * meet at the point of intersection.
     * The tricky part is to recognize that the lengths can be the same, in which case, we need to ensure that the two
     * pointers don't end up pointing to the same list.
     */
    public static <T> Node<T> getIntersectionNode(Node<T> headA, Node<T> headB) {
        if (headA == null || headB == null) {
            return null;
        }
        int l1 = length(headA);
        int l2 = length(headB);
        int diff = Math.abs(l1 - l2);
        Node<T> longer = l1 > l2 ? headA : headB;

        while (diff > 0) {
            longer = longer.next;
            diff--;
        }

        Node<T> shorter = l1 > l2 ? headB : headA;
        while (longer != null && shorter != null && longer != shorter) {
            longer = longer.next;
            shorter = shorter.next;
        }
        return shorter;
    }

    private static <T> int length(Node<T> head) {
        Node<T> cur = head;
        int l = 0;

        while (cur != null) {
            cur = cur.next;
            l++;
        }
        return l;
    }
}
