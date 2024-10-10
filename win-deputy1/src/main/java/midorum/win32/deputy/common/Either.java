package midorum.win32.deputy.common;

import dma.function.ConsumerThrowing;
import dma.function.SupplierThrowing;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class Either<T, X extends Throwable> {

    private final T value;
    private final X error;

    public Either(final T value, final X error) {
        this.value = value;
        this.error = error;
    }

    private static <T, X extends Throwable> Either<T, X> fromValue(final T value) {
        return new Either<>(Objects.requireNonNull(value), null);
    }

    public static <T, X extends Throwable> Either<T, X> error(final X error) {
        return new Either<>(null, Objects.requireNonNull(error));
    }

    public static <T, X extends Throwable> FromResultSupplier<T, X> value(final SupplierThrowing<T, X> supplier) {
        return new FromResultSupplier<>(supplier);
    }

    public T getOrThrow() throws X {
        if (error != null) throw error;
        return value;
    }

    public <E extends Throwable> T getOrThrow(Supplier<? extends E> exceptionSupplier) throws E {
        if (error != null) {
            final E exception = exceptionSupplier.get();
            exception.addSuppressed(error);
            throw exception;
        }
        return value;
    }

    public <E extends Throwable> T getOrThrow(Function<X, ? extends E> exceptionSupplier) throws E {
        if (error != null) {
            final E exception = exceptionSupplier.apply(error);
            exception.addSuppressed(error);
            throw exception;
        }
        return value;
    }

    public T getOrHandleError(final Function<X, T> errorHandler) {
        if (error == null) return value;
        return errorHandler.apply(error);
    }

    public Stream<T> streamOrThrow() throws X {
        return Stream.of(getOrThrow());
    }

    public <E extends Throwable> Stream<T> streamOrThrow(Supplier<? extends E> exceptionSupplier) throws E {
        return Stream.of(getOrThrow(exceptionSupplier));
    }

    public Stream<T> streamOrHandleError(final Function<X, T> errorHandler) {
        return Stream.of(getOrHandleError(errorHandler));
    }

    public Either<Optional<T>, X> filter(Predicate<? super T> predicate) {
        Objects.requireNonNull(predicate);
        if (error != null) return error(error);
        return predicate.test(value) ? fromValue(Optional.of(value)) : fromValue(Optional.empty());
    }

    public <U> Either<U, X> map(Function<? super T, ? extends U> mapper) {
        Objects.requireNonNull(mapper);
        if (error != null) return error(error);
        else return fromValue(mapper.apply(value));
    }

    public <U> Either<U, X> flatMap(Function<? super T, ? extends Either<? extends U, X>> mapper) {
        Objects.requireNonNull(mapper);
        if (error != null) return error(error);
        @SuppressWarnings("unchecked") final Either<U, X> apply = (Either<U, X>) mapper.apply(value);
        return Objects.requireNonNull(apply);
    }

    public <E extends Throwable> void consumeOrThrow(ConsumerThrowing<? super T, E> consumer) throws E, X {
        if (error != null) throw error;
        consumer.accept(value);
    }

    public void consumeOrHandleError(Consumer<? super T> consumer, final Consumer<X> errorHandler) {
        if (error != null) {
            errorHandler.accept(error);
            return;
        }
        consumer.accept(value);
    }

    public <E extends Throwable> void consumeOrHandleError(ConsumerThrowing<? super T, E> consumer, final ConsumerThrowing<X, E> errorHandler) throws E {
        if (error != null) {
            errorHandler.accept(error);
            return;
        }
        consumer.accept(value);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (!(o instanceof final Either<?, ?> either)) return false;
        return Objects.equals(value, either.value) && Objects.equals(error, either.error);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value, error);
    }

    @Override
    public String toString() {
        return "Either{" +
                "value=" + value +
                ", error=" + error +
                '}';
    }

    public static class FromResultSupplier<T, X extends Throwable> {

        private final SupplierThrowing<T, X> supplier;

        private FromResultSupplier(final SupplierThrowing<T, X> supplier) {
            this.supplier = supplier;
        }

        @SuppressWarnings("unchecked")
        public Either<T, X> orException() {
            Either<T, X> result;
            try {
                result = fromValue(supplier.get());
            } catch (Throwable e) {
                switch (e) {
                    case RuntimeException re -> throw re;
                    case Error err -> throw err;
                    default -> result = error((X) e);
                }
            }
            return result;
        }

        public Either<T, X> orException(final Supplier<X> exceptionSupplier) {
            try {
                return fromValue(supplier.get());
            } catch (Throwable e) {
                switch (e) {
                    case RuntimeException re -> throw re;
                    case Error err -> throw err;
                    default -> {
                        final X error = exceptionSupplier.get();
                        error.addSuppressed(e);
                        return error(error);
                    }
                }
            }
        }

        public Either<T, X> orError(final Supplier<X> exceptionSupplier) {
            try {
                return fromValue(supplier.get());
            } catch (Throwable e) {
                final X error = exceptionSupplier.get();
                error.addSuppressed(e);
                return error(error);
            }
        }

    }
}
