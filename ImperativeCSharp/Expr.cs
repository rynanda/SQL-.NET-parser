public abstract class Expr 
{
    public abstract SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr);
}

public class ColumnReference : Expr 
{ 
    public string columnName { get; set; }

    public ColumnReference(string columnName)
    {
        this.columnName = columnName;
    }

    public static SQLValue evalColumnReference (List<ColumnInfo> columnsInfo, Row row, string columnName)
    {
        throw new NotImplementedException();
    }

    public static string? getColumnNameFromColumnRef(Expr expr)
    {
        throw new NotImplementedException();
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class QualifiedColumnReference : Expr
{
    public string tableName { get; set; }
    public string columnName { get; set; }

    public QualifiedColumnReference(string tableName, string columnName)
    {
        this.tableName = tableName;
        this.columnName = columnName;
    }

    public static SQLValue evalQualifiedColumnReference (List<ColumnInfo> columnsInfo, string tableName, string columnName)
    {
        throw new NotImplementedException();
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class IntLiteral : Expr
{
    public int intValue { get; set; }

    public IntLiteral(int intValue)
    {
        this.intValue = intValue;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class StringLiteral : Expr
{
    public string stringValue { get; set; }

    public StringLiteral(string stringValue)
    {
        this.stringValue = stringValue;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Parenthesis : Expr
{
    public Expr subexpr { get; set; }

    public Parenthesis(Expr subexpr)
    {
        this.subexpr = subexpr;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Negative : Expr
{
    public Expr subexpr { get; set; }

    public Negative(Expr subexpr)
    {
        this.subexpr = subexpr;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Positive : Expr
{
    public Expr subexpr { get; set; }
    public Positive(Expr subexpr)
    {
        this.subexpr = subexpr;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Not : Expr
{
    public Expr subexpr { get; set; }

    public Not(Expr subexpr)
    {
        this.subexpr = subexpr;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Addition : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public Addition(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Subtract : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public Subtract(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Multiply : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public Multiply(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Divide : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public Divide(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class LessThan : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public LessThan(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class GreaterThan : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }
    
    public GreaterThan(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class LessThanOrEqual : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public LessThanOrEqual(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class GreaterThanOrEqual : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public GreaterThanOrEqual(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class And : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public And(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Or : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public Or(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Equals : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public Equals(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class NotEquals : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public NotEquals(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Like : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public Like(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class In : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public In(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class NotIn : Expr
{
    public Expr lhs { get; set; }
    public Expr rhs { get; set; }

    public NotIn(Expr lhs, Expr rhs)
    {
        this.lhs = lhs;
        this.rhs = rhs;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Max : Expr
{
    public Expr expr { get; set; }

    public Max(Expr expr)
    {
        this.expr = expr;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Min : Expr
{
    public Expr expr { get; set; }

    public Min(Expr expr)
    {
        this.expr = expr;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Count : Expr
{
    public Expr expr { get; set; }

    public Count(Expr expr)
    {
        this.expr = expr;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Subquery : Expr
{
    public Query query { get; set; }

    public Subquery(Query query)
    {
        this.query = query;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}

public class Exists : Expr
{
    public Query query { get; set; }

    public Exists(Query query)
    {
        this.query = query;
    }
    public override SQLValue computeExprInRow(List<ColumnInfo> columnsInfo, Row row, Expr expr)
    {
        throw new NotImplementedException();
    }
}