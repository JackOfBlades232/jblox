#include <defs.hpp>
#include <version.hpp>
#include <print>

int main()
{
    u32 const major_ver = MAJOR_VERSION, minor_ver = MINOR_VERSION;
    std::println(
        "Hello, world! This is jb-cpplox, version {} (major={}, minor={}).",
        VERSION, major_ver, minor_ver);
}
