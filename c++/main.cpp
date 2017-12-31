#include <iostream>

class Foo {
public:
    Foo(int i) : _i(i) {
        std::cout << "constructor is called" << std::endl;
    }

    Foo(const Foo& foo) : _i(foo._i) {
        std::cout << "copy constructor is called" << std::endl;
    }

    Foo &operator=(const Foo& foo) {
        std::cout << "copy assignment operator is called" << std::endl;

        _i = foo._i;
        return *this;
    }

    Foo(Foo&& foo) : _i(foo._i) {
        std::cout << "move constructor is called" << std::endl;
    }

    Foo &operator=(Foo&& foo) {
        std::cout << "move assignment operator is called" << std::endl;

        _i = foo._i;
        return *this;
    }

    int _i;
};


class Bar {
public:
    void foobar(Foo foo) {
        std::cout << "foobar called" << std::endl;
    };
};

int main(int, char **) {
    Foo foo(42);
    Bar bar;

    bar.foobar(foo);

    std::cout << "----------------------------------" << std::endl;

    bar.foobar(std::move(foo));
}
