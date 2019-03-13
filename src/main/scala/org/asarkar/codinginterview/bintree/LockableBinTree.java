package org.asarkar.codinginterview.bintree;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

/*
 * Implement locking in a binary tree. A binary tree node can be locked or unlocked only if all of its descendants or
 * ancestors are not locked.
 *
 * Design a binary tree node class with the following methods:
 * - is_locked, which returns whether the node is locked
 * - lock, which attempts to lock the node. If it cannot be locked, then it should return false. Otherwise, it should
 *   lock it and return true.
 * - unlock, which unlocks the node. If it cannot be unlocked, then it should return false. Otherwise, it should unlock
 *   it and return true.
 *
 * You may augment the node to add parent pointers or any other property you would like. You may assume the class is
 * used in a single-threaded program, so there is no need for actual locks or mutexes. Each method should run in O(h),
 * where h is the height of the tree.
 *
 * ANSWER: This is a weird question, because once the tree is locked, it can't be unlocked. In order to avoid visiting
 * all the nodes during locking and unlocking, we indicate a node is unlocked only if all of it's children are unlocked.
 * That way, we don't have to traverse the subtrees, only the ancestors.
 */
class LockableBinTree<E> extends Node<E> {
    LockableBinTree<E> parent;
    private boolean locked;

    LockableBinTree(LockableBinTree<E> left, LockableBinTree<E> right, E datum) {
        super(left, right, datum);
    }

    boolean isLocked() {
        return locked &&
                Optional.ofNullable(parent)
                        .map(LockableBinTree::isLocked)
                        .orElse(true);
    }

    boolean lock() {
        System.out.printf("Locking node: %s%n", datum);
        return (locked = lockOrUnlock(LockableBinTree::lock));
    }

    boolean unlock() {
        System.out.printf("Unlocking node: %s%n", datum);
        return !(locked = !lockOrUnlock(LockableBinTree::unlock));
    }

    private boolean lockOrUnlock(Function<LockableBinTree<E>, Boolean> fn) {
        if (locked) {
            System.out.printf("Node: %s is already locked%n", datum);
            return false;
        } else {
            return Stream.of(left, right)
                    .filter(Objects::nonNull)
                    .map(child -> fn.apply(((LockableBinTree<E>) child)))
                    .filter(locked -> !locked)
                    .findFirst()
                    .orElse(true);
        }
    }
}
