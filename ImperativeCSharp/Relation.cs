// Information about the relation

public class TableReference
{
    public string name { get; set; }
    public string? alias { get; set; }

    public TableReference(string name, string? alias)
    {
        this.name = name;
        this.alias = alias;
    }

    public static TableReference getTableRefs(TableList? tables)
    {
        throw new NotImplementedException();
    }
}

public class TableList
{
    public TableReference firstTable { get; set; }
    public List<JoinClause> remainingTables { get; set; }

    public TableList(TableReference firstTable, List<JoinClause> remainingTables)
    {
        this.firstTable = firstTable;
        this.remainingTables = remainingTables;
    }
}

public class ResultColumn
{
    public Expr expr { get; set; }
    public string columnAlias { get; set; }
    public string tableName { get; set; }

    public ResultColumn(Expr expr, string columnAlias, string tableName)
    {
        this.expr = expr;
        this.columnAlias = columnAlias;
        this.tableName = tableName;
    }
}

public class JoinClause
{
    public joinType joinOperator { get; set; }
    public TableReference rightTableName { get; set; }
    public Expr joinConstraint { get; set; }

    public JoinClause(joinType joinOperator, TableReference rightTableName, Expr joinConstraint)
    {
        this.joinOperator = joinOperator;
        this.rightTableName = rightTableName;
        this.joinConstraint = joinConstraint;
    }
}

public enum joinType
{
    LEFT, RIGHT, INNER, CROSS
}

public class Row
{
    public List<SQLValue> SQLValues { get; set; }

    public Row(List<SQLValue> SQLValues) 
    {
        this.SQLValues = SQLValues;
    }
}

public class ColumnInfo
{
    public string columnName { get; set; }
    public TableReference? src { get; set; }

    public ColumnInfo(string columnName, TableReference? src)
    {
        this.columnName = columnName;
        this.src = src;
    }
}

public class Relation
{
    public List<ColumnInfo> columnsInfo { get; set; }
    public List<Row> rows { get; set; }
    public List<List<Row>>? groups { get; set; }

    public Relation(List<ColumnInfo> columnsInfo, List<Row> rows, List<List<Row>>? groups)
    {
        this.columnsInfo = columnsInfo;
        this.rows = rows;
        this.groups = groups;
    }

    // Used in performFrom
    public static Relation join(Relation leftTable, JoinClause joinClause)
    {
        throw new NotImplementedException();
    }

    // Perform a From clause
    public static Relation performFrom(TableList? tables)
    {
        throw new NotImplementedException();
    }

    // Perform a Where clause
    public static Relation performWhere(Expr? condition, Relation table)
    {
        throw new NotImplementedException();
    }
    
    // Used in performSelect below
    public static List<ColumnInfo>? getColumnInfos(ResultColumn resultColumn)
    {
        throw new NotImplementedException();
    }
    
    // Perform a Select clause
    public static Relation performSelect(List<ResultColumn> selectResults, Relation table)
    {
        throw new NotImplementedException();
    }

    // Perform a Distinct clause
    public static Relation performDistinct(bool distinct, Relation table)
    {
        throw new NotImplementedException();
    }

    // Perform a Group By clause
    public static Relation performGroupBy(List<Expr>? groupByColumns, Relation table)
    {
        throw new NotImplementedException();
    }

    // Perform a Having clause
    public static Relation performHaving(Expr? condition, Relation table)
    {
        throw new NotImplementedException();
    }

    // Perform an OrderBy clause
    public static Relation performOrderBy(List<Expr>? orderbyexpressions, Relation table)
    {
        throw new NotImplementedException();
    }

    // Perform a Limit clause
    public static Relation performLimit(int? limit, Relation table)
    {
        throw new NotImplementedException();
    }
}