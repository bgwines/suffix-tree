//
//  suffix-tree.h
//  suffix-tree
//
//  Created by Brett Wines on 1/5/13.
//  Copyright (c) 2013 Brett Wines. All rights reserved.
//

#ifndef __suffix_tree__suffix_tree__
#define __suffix_tree__suffix_tree__

#include <iostream>

using namespace std;

class suffix_tree
{
 public:
    suffix_tree(string str);
    ~suffix_tree();
    
 private:
    void build(string str);
};

#endif /* defined(__suffix_tree__suffix_tree__) */
