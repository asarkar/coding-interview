package org.asarkar.codinginterview.lists;

import java.util.Deque;

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
        if (head == null) {
            return true;
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

        while (next != null) {
            if (cur.datum != next.datum) {
                return false;
            }
            cur = cur.next;
            next = next.next;
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

    /*
     * Given k sorted singly linked lists, write a function to merge all the lists into one sorted singly linked list.
     *
     * ANSWER: Lets assume for the sake of simplicity that all the lists are of length n. If we extend the idea of
     * merging two sorted lists from merge sort, and recursively merge pairs of lists, we do O(2n) work for the first
     * pair, O(3n) pair for the second pair, and so on. Since there are k - 1 pairs, the sum is 2n + 3n + ... +
     * (k - 1)n, or n(1 + 2 + .. + k) - 2n = nk(k + 1)/2 - 2n. This is O(nk^2) time.
     *
     * We can improve on this by observing that the quadratic time in k comes from the fact that the size of one of
     * the lists to be merged grows with each iteration. If we could keep that constant, we would be doing constant
     * work per iteration. If instead of recursively merging the a list with the result of the previous iteration, we
     * merge the first list with the second, the third with the fourth, and so on, we do O(2n) work for merging each
     * pair. Since the size of the problem halves each times, in the first iteration, we do O(2n * k/2) = O(nk) work,
     * second iteration O(4n * k/4) = O(nk) work, and so on. Overall time complexity is O(nk log(k)).
     *
     * The same time complexity can also be achieved by using a min priority queue.
     */
    public static Node<Integer> mergeKLists(Node<Integer>[] lists) {
        if (lists == null || lists.length == 0) {
            return null;
        }
        Deque<Node<Integer>> merged = new java.util.LinkedList<>();

        for (int i = 0; i < lists.length; i += 2) {
            if (i + 1 >= lists.length) {
                merged.push(lists[i]);
            } else {
                merged.push(merge2Lists(lists[i], lists[i + 1]));
            }
        }

        while (merged.size() > 1) {
            // using push instead of offer would be O(nk^2) time
            merged.offer(merge2Lists(merged.pop(), merged.pop()));
        }

        return merged.pop();
    }

    /*
     * The basic idea is similar to the merge step in merge sort; we keep a pointer corresponding to each input list;
     * at each iteration, we advance the pointer corresponding to the smaller element. However, there's one crucial
     * difference where most people get tripped. In merge sort, since we use a result array, the next position to
     * insert is always the index of the result array. For a linked list, we need to keep a pointer to the last
     * element of the sorted list. The pointer may jump around from one input list to another depending on which one
     * has the smaller element for the current iteration.
     *
     * With that, the following code should be self-explanatory. Time complexity: O(m + n). Space complexity: O(1).
     */
    public static Node<Integer> merge2Lists(Node<Integer> l1, Node<Integer> l2) {
        if (l1 == null) {
            return l2;
        }
        if (l2 == null) {
            return l1;
        }
        Node<Integer> first = l1;
        Node<Integer> second = l2;
        Node<Integer> head = null;
        Node<Integer> last = null;

        while (first != null && second != null) {
            if (first.datum < second.datum) {
                if (last != null) {
                    last.next = first;
                }
                last = first;
                first = first.next;
            } else {
                if (last != null) {
                    last.next = second;
                }
                last = second;
                second = second.next;
            }
            if (head == null) {
                head = last;
            }
        }

        if (first == null) {
            last.next = second;
        }
        if (second == null) {
            last.next = first;
        }

        return head;
    }
}
