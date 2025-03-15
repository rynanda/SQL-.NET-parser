public class Query
{
    public bool distinct { get; set; }
    public List<ResultColumn> select { get; set; }
    public TableList from { get; set; }
    public Expr where { get; set; }
    public List<Expr> groupby { get; set; }
    public Expr having { get; set; }
    public List<Expr> orderby { get; set; }
    public int limit { get; set; }

    public Query(bool distinct, List<ResultColumn> select, TableList from, Expr where, List<Expr> groupby, Expr having, List<Expr> orderby, int limit)
    {
        this.distinct = distinct;
        this.select = select;
        this.from = from;
        this.where = where;
        this.groupby = groupby;
        this.having = having;
        this.orderby = orderby;
        this.limit = limit;
    } 

    // Execute a query
    public static Relation Execute(Query ast)
    {
        throw new NotImplementedException();
    }

    // Parse then execute query
    public static Query Run(string queryString)
    {
        throw new NotImplementedException();
    }

    // Display formatted result of the query
    public static void Display(string queryString)
    {
        throw new NotImplementedException();
    }
}