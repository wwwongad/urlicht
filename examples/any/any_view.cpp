#include <urlicht/any/any_view.h>

#include <iostream>
#include <string>
#include <vector>

int main() {
    int status_code = 200;
    std::string request_id = "req-42";
    std::vector<urlicht::any::any_view> fields{status_code, request_id};

    status_code = 201;  // Views observe the original objects rather than owning copies.

    std::cout << "status=" << urlicht::any::any_cast<int>(fields[0])
              << ", request=" << urlicht::any::any_cast<std::string>(fields[1]) << '\n';
}
