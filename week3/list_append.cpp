#include <iostream>

struct Node
{
    int value;
    Node *next;

    Node(int _v)
        : value(_v), next(nullptr) {}

    void print()
    {
        std::cout << value << ", ";
        if (next)
            next->print();
        std::cout << std::endl;
    }
};

void merge(Node *l1, Node *l2)
{
    if (!l1)
        return;
    if (!l1->next)
    {
        l1->next = l2;
        return;
    }

    merge(l1->next, l2);
}

int main()
{
    Node *headOne = new Node(1);
    Node *nodeOneOne = new Node(2);
    Node *nodeOneTwo = new Node(3);

    headOne->next = nodeOneOne;
    nodeOneOne->next = nodeOneTwo;

    Node *headTwo = new Node(4);
    Node *nodeTwoOne = new Node(5);
    Node *nodeTwoTwo = new Node(6);

    headTwo->next = nodeTwoOne;
    nodeTwoOne->next = nodeTwoTwo;

    headOne->print();
    headTwo->print();

    merge(headOne, headTwo);
    headOne->print();
}