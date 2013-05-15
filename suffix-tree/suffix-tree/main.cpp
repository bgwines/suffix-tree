//
//  main.cpp
//  suffix-tree
//
//  Created by Brett Wines on 1/5/13.
//  Copyright (c) 2013 Brett Wines. All rights reserved.
//

#include "suffix-tree.h"
#include <iostream>

int main(int argc, const char * argv[]) {
    string test_cases[] = {"abc", "happy", "bananas", "abcabxabcd"};
    for (int i=0; i<1; i++) {
        cout << "\ntest case #" << i << ": \"" << test_cases[i] << "\"\n";
        suffix_tree tree(test_cases[i]);
        tree.print_tree();
    }
    
    return 0;
}

