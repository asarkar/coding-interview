package org.asarkar.codinginterview.design;

import java.io.Serializable;
import java.util.concurrent.atomic.AtomicInteger;

/*
 * Implement the singleton pattern with a twist. First, instead of storing one instance, store two instances. And in
 * every even call of getInstance(), return the first instance and in every odd call of getInstance(), return the
 * second instance.
 *
 * ANSWER:
 * See:
 * https://www.oracle.com/technetwork/articles/java/singleton-1577166.html
 * https://medium.com/@kevalpatel2106/how-to-make-the-perfect-singleton-de6b951dfdb0
 * https://www.cs.umd.edu/~pugh/java/memoryModel/DoubleCheckedLocking.html
 */
public final class Singleton implements Cloneable, Serializable {
    private static AtomicInteger counter = new AtomicInteger(1);

    public static Singleton getInstance() {
        if (counter.getAndIncrement() % 2 == 0) {
            return EvenHelper.instance;
        } else {
            return OddHelper.instance;
        }
    }

    private static class EvenHelper {
        // not initialized until the class is used in getInstance()
        static Singleton instance = new Singleton();
    }

    private static class OddHelper {
        // not initialized until the class is used in getInstance()
        static Singleton instance = new Singleton();
    }

    // make singleton from deserializaion
    protected Singleton readResolve() {
        return getInstance();
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        throw new CloneNotSupportedException("Use getInstance() instead");
    }
}
