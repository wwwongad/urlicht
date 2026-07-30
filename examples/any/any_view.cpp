#include <urlicht/any/any_view.h>

#include <iostream>
#include <string>
#include <unordered_map>
#include <map>
#include <vector>


enum class viewed_t {
    vector,
    map,
    unordered_map
};

class observer {
    viewed_t type_;
    urlicht::any::any_view view_;
public:
    template <typename T>
    observer(const viewed_t type, const T& obj) : type_(type), view_(obj) {}

    template <typename T>
    void view(const viewed_t type, const T& obj) {
        type_ = type;
        view_.view(obj);
    }

    void on_change() const {
        switch (type_) {
            case viewed_t::vector: {
                std::cout << "vector: ";
                for (const int elem : urlicht::any::any_cast<std::vector<int>>(view_)) {
                    std::cout << elem << " ";
                }
                break;
            }
            case viewed_t::map: {
                std::cout << "map: ";
                const auto& map = urlicht::any::any_cast<std::map<int, std::string>>(view_);
                for (const auto& [key, value] : map) {
                    std::cout << '[' << key << ", " << value << "] ";
                }
                break;
            }
            case viewed_t::unordered_map: {
                std::cout << "unordered map: ";
                const auto& map = urlicht::any::any_cast<std::unordered_map<int, std::string>>(view_);
                for (const auto& [key, value] : map) {
                    std::cout << '[' << key << ", " << value << "] ";
                }
                break;
            }
        }
        std::cout << std::endl;
    }
};

int main() {
    std::vector vec = {10, 20, 30};
    std::map<int, std::string> m = {{1, "one"}, {2, "two"}};
    std::unordered_map<int, std::string> um = {{3, "three"}, {4, "four"}};

    observer obs1(viewed_t::vector, vec);
    observer obs2(viewed_t::map, m);
    observer obs3(viewed_t::unordered_map, um);

    std::cout << "Initial state:\n";
    obs1.on_change();
    obs2.on_change();
    obs3.on_change();

    vec.push_back(40);
    m[5] = "five";
    um[6] = "six";

    std::cout << "\nAfter modifications:\n";
    obs1.on_change();
    obs2.on_change();
    obs3.on_change();

    obs1.view(viewed_t::unordered_map, um);
    obs2.view(viewed_t::vector, vec);
    obs3.view(viewed_t::map, m);

    std::cout << "\nChanged viewed subject: \n";
    obs1.on_change();
    obs2.on_change();
    obs3.on_change();
}
