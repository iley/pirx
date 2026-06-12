// Same basename as the build directory: in directory builds the Pirx object
// is named after the directory, so this C file's object must not collide
// with it.
int c_scale(int x) {
    return x * 10;
}
