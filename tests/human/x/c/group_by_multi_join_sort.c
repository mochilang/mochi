// group_by_multi_join_sort.c - manual translation of tests/vm/valid/group_by_multi_join_sort.mochi
#include <stdio.h>
#include <string.h>

struct Nation { int n_nationkey; const char *n_name; };
struct Customer {
    int c_custkey; const char *c_name; double c_acctbal; int c_nationkey;
    const char *c_address; const char *c_phone; const char *c_comment;
};
struct Order { int o_orderkey; int o_custkey; const char *o_orderdate; };
struct LineItem { int l_orderkey; const char *l_returnflag; double l_extendedprice; double l_discount; };

int main() {
    struct Nation nation[] = { {1, "BRAZIL"} };
    struct Customer customer[] = {
        {1, "Alice", 100.0, 1, "123 St", "123-456", "Loyal"}
    };
    struct Order orders[] = {
        {1000, 1, "1993-10-15"},
        {2000, 1, "1994-01-02"}
    };
    struct LineItem lineitem[] = {
        {1000, "R", 1000.0, 0.1},
        {2000, "N", 500.0, 0.0}
    };

    const char *start_date = "1993-10-01";
    const char *end_date   = "1994-01-01";

    double revenue = 0.0;
    const struct Customer *gc = NULL;
    const struct Nation *gn = NULL;

    for (int ci = 0; ci < 1; ci++) {
        for (int oi = 0; oi < 2; oi++) if (orders[oi].o_custkey == customer[ci].c_custkey) {
            for (int li = 0; li < 2; li++) if (lineitem[li].l_orderkey == orders[oi].o_orderkey) {
                for (int ni = 0; ni < 1; ni++) if (nation[ni].n_nationkey == customer[ci].c_nationkey) {
                    if (strcmp(orders[oi].o_orderdate, start_date) >= 0 &&
                        strcmp(orders[oi].o_orderdate, end_date) < 0 &&
                        strcmp(lineitem[li].l_returnflag, "R") == 0) {
                        revenue += lineitem[li].l_extendedprice * (1 - lineitem[li].l_discount);
                        gc = &customer[ci];
                        gn = &nation[ni];
                    }
                }
            }
        }
    }

    if (gc) {
        printf("map[c_acctbal:%.0f c_address:%s c_comment:%s c_custkey:%d c_name:%s c_phone:%s n_name:%s revenue:%.0f]\n",
               gc->c_acctbal, gc->c_address, gc->c_comment, gc->c_custkey,
               gc->c_name, gc->c_phone, gn->n_name, revenue);
    }
    return 0;
}
