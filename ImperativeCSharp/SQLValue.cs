public abstract class SQLValue 
{
    public static SQLValue boolToSQLValue(bool value)
    {
        throw new NotImplementedException();
    }

    public static bool toBool(SQLValue value)
    {
        throw new NotImplementedException();
    }
}

public class SQLIntValue : SQLValue 
{
    public int Value { get; set; }

    public SQLIntValue(int value) 
    {
        Value = value;
    }
}

public class SQLVarcharValue : SQLValue
{
    public string Value { get; set; }

    public SQLVarcharValue(string value) 
    { 
        Value = value; 
    }
}

public class SQLNull : SQLValue
{
    public string? NullValue { get; }
    public SQLNull()
    {
        NullValue = null;
    }
}