module Execution

// Execute an SQL Query (expressed via an AST) to produce a Relational result

open AST
open IO
open Parser
open RuntimeTypes

exception TypeError of string

// Dummy values to use as placeholders until you fix/correctly implement the functions below.
let dummySQLValue = SQLNull
let dummyRelation = {columnsInfo=[]; rows= Seq.empty; groups=None}
let dummySQLValueList = []

// Evaluate a column reference
let evalColumnReference (columnsInfo:ColumnInfo list) (row:Row) (columnName:string) : SQLValue =
    
    // Find the index of columns in a list of column infos that match the columnName input string
    let findResult : int option = 
        columnsInfo 
        |> Seq.tryFindIndex (fun (c:ColumnInfo) -> columnName = c.columnName)

    // Match the (if found) index and return SQLValue from row
    match findResult with
    | Some(index) -> row.[index]
    | None -> SQLNull

// Evaluate a qualified column reference (column name preceded by table name)
let evalQualifiedColumnReference (columnsInfo:ColumnInfo list) (row:Row) (tableName:string) (columnName:string) : SQLValue =
    
    // Check if optional ColumnInfo.src matches the input tableName string
    let checkTable (c:ColumnInfo) : bool =
        match c.src with
        | Some(tableRef) -> tableRef.name = tableName
        | None -> false

    // Calls checkTable and checks if a ColumnInfo.columnName matches the columnName input string. Returns true if both match
    let checkColumn (c:ColumnInfo) : bool =
        (checkTable c) && (c.columnName = columnName)

    // Match the (if found) index and return SQLValue from row
    let findResult : int option = 
        columnsInfo 
        |> Seq.tryFindIndex checkColumn
    match findResult with
    | Some(index) -> row.[index]
    | None -> SQLNull

// Converts a bool value (true/false) to SQLValue (1/0)
let boolToSQLValue (value:bool) : SQLValue =
    SQLIntValue(if value then 1 else 0)

// Converts an SQLValue (1/0) to a bool value (true/false)
let toBool (value:SQLValue) : bool =
    match value with
    | SQLIntValue(x) -> x = 1
    | _ -> false

// A helper function to evaluate an expression for a particular row given the names of each column
let rec computeExprInRow (columnsInfo: ColumnInfo list) (row:Row) (expr:Expr) : SQLValue =
    match expr with
    | ColumnReference (columnName:string) -> (evalColumnReference columnsInfo row columnName)

    | QualifiedColumnReference (tableName:string, columnName:string) -> (evalQualifiedColumnReference columnsInfo row tableName columnName)

    | IntLiteral (x:int) -> SQLIntValue(x)

    | StringLiteral (s:string) -> SQLVarcharValue(s)

    | Parenthesis (subExpr:Expr) -> (computeExprInRow columnsInfo row subExpr)

    | Negative (subExpr:Expr) ->
        let subValue = computeExprInRow columnsInfo row subExpr
        match subValue with
        | SQLIntValue(x) -> SQLIntValue(-x)
        | _ -> SQLNull

    | Positive (subExpr:Expr) -> computeExprInRow columnsInfo row subExpr

    | Not (subExpr:Expr) ->
        let subValue = computeExprInRow columnsInfo row subExpr
        boolToSQLValue(not(toBool subValue))

    | Addition (lhs:Expr, rhs:Expr) ->
        
        // Recursively compute the lhs and rhs expressions
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs

        // Add the computed lhs and rhs using a match expression
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            SQLIntValue ((+) intValue1 intValue2)
        | _ -> SQLNull

        // Same format for following Expr matches

    | Subtract (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue(intValue1), SQLIntValue(intValue2) ->
            SQLIntValue ((-) intValue1 intValue2)
        | _ -> SQLNull

    | Multiply (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            SQLIntValue ((*) intValue1 intValue2)
        | _ -> SQLNull

    | Divide (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            SQLIntValue ((/) intValue1 intValue2)
        | _ -> SQLNull

    | LessThan (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            if intValue1 < intValue2 then SQLIntValue 1
            else SQLIntValue 0
        | SQLVarcharValue strValue1, SQLVarcharValue strValue2 ->
            if strValue1 < strValue2 then SQLIntValue 1
            else SQLIntValue 0
        | _ -> SQLNull

    | GreaterThan (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            if intValue1 > intValue2 then SQLIntValue 1
            else SQLIntValue 0
        | SQLVarcharValue strValue1, SQLVarcharValue strValue2 ->
            if strValue1 > strValue2 then SQLIntValue 1
            else SQLIntValue 0
        | _ -> SQLNull

    | LessThanOrEqual (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            if intValue1 <= intValue2 then SQLIntValue 1
            else SQLIntValue 0
        | SQLVarcharValue strValue1, SQLVarcharValue strValue2 ->
            if strValue1 <= strValue2 then SQLIntValue 1
            else SQLIntValue 0
        | _ -> SQLNull

    | GreaterThanOrEqual (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            if intValue1 >= intValue2 then SQLIntValue 1
            else SQLIntValue 0
        | SQLVarcharValue strValue1, SQLVarcharValue strValue2 ->
            if strValue1 >= strValue2 then SQLIntValue 1
            else SQLIntValue 0
        | _ -> SQLNull

    | Equals (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            if intValue1 = intValue2 then SQLIntValue 1
            else SQLIntValue 0
        | SQLVarcharValue strValue1, SQLVarcharValue strValue2 ->
            if strValue1 = strValue2 then SQLIntValue 1
            else SQLIntValue 0
        | _ -> SQLNull

    | NotEquals (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            if intValue1 <> intValue2 then SQLIntValue 1
            else SQLIntValue 0
        | SQLVarcharValue strValue1, SQLVarcharValue strValue2 ->
            if strValue1 <> strValue2 then SQLIntValue 1
            else SQLIntValue 0
        | _ -> SQLNull

    | And (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            if toBool(SQLIntValue(intValue1)) && toBool(SQLIntValue(intValue2)) then SQLIntValue 1
            else SQLIntValue 0
        | _ -> SQLNull

    | Or (lhs:Expr, rhs:Expr) ->
        let lhsValue = computeExprInRow columnsInfo row lhs
        let rhsValue = computeExprInRow columnsInfo row rhs
        match lhsValue, rhsValue with
        | SQLIntValue intValue1, SQLIntValue intValue2 ->
            if toBool(SQLIntValue(intValue1)) || toBool(SQLIntValue(intValue2)) then SQLIntValue 1
            else SQLIntValue 0
        | SQLNull, SQLIntValue 1 -> SQLIntValue 1
        | SQLIntValue 1, SQLNull -> SQLIntValue 1
        | _ -> SQLNull

    | _ -> SQLNull

// TODO: add other (recursively used) helper functions as required.

// Get a TableReference from a list of tables
let rec getTableRefs (tables:TableList option) : TableReference =
    match tables with
    | Some tableList -> tableList.firstTable
    | None -> {name="None";alias=None}

// Used in performFrom below             
and join (leftTable:Relation) (joinClause: JoinClause) : Relation  =
    dummyRelation // TODO: fix/replace this dummy/placeholder result

// Perform a From clause
and performFrom (tables:TableList option) : Relation = 
    match tables with
    | Some tableList -> LoadTable(getTableRefs(Some(tableList)))
    | None -> {columnsInfo=[]; rows= seq {yield []}; groups=None} // Return empty Relation if no TableList

// Perform a Where clause
and performWhere (condition:Expr option) (table:Relation) : Relation =
    match condition with
    | Some expr ->

        // If some expression exists, filter the rows based on this expression
        let filterRows : Row seq = 
            table.rows
            |> Seq.filter (fun row ->
                let boolResult = computeExprInRow table.columnsInfo row expr
                match boolResult with
                | SQLIntValue 1 -> true
                | _ -> false )

        // Return a new table with filtered rows
        {table with rows = filterRows}

    | None -> table // If no expression, return original table

// Get a column name from a column reference expression
and getColumnNameFromColumnRef (expr:Expr) : string option =
        match expr with
        | ColumnReference columnName -> Some columnName
        | _ -> None

// Perform a Select clause
and performSelect (selectResults: ResultColumn list) (table:Relation) : Relation =

    // Recursively get column infos based on ResultColumns in selectResults
    let rec getColumnInfos (resultColumn: ResultColumn) : ColumnInfo list option =
        match resultColumn with
        | Star -> 
            Some table.columnsInfo // Return all column infos in the original relation without modification

        // Return column infos in a specific table in the original relation
        | TableStar tableName ->

            // Filter the column infos based on table name
            let filterColumns =
                table.columnsInfo
                |> List.filter (fun columnInfo -> columnInfo.columnName.StartsWith(tableName))

            // If no columns pass the filter, return None, else return the tableName column infos
            match filterColumns with
            | [] -> None
            | _ -> Some [{ columnName = tableName; src = Some {name = tableName; alias = None}}] 
        
        // Compute column info expressions
        | ResultExpr (expr, alias) ->
            
            // Get optional aliases
            let optAlias =
                match alias with
                | Some aliasName -> Some aliasName
                | None -> None

            // Match given expressions
            match expr with
            | ColumnReference columnName ->

                // Try to find matching column infos based on a column name
                match getColumnNameFromColumnRef expr with
                | Some resultColumnName ->
                    let tryFindColumns : ColumnInfo option =
                        table.columnsInfo 
                        |> List.tryFind (fun columnInfo -> columnInfo.columnName = resultColumnName)

                    // If matching column infos were found, return a list of column infos
                    // with the column name, original src, and optional alias
                    match tryFindColumns with
                    | Some columnInfo -> Some [{columnName = resultColumnName; src = Some{name = columnInfo.src.Value.name; alias = optAlias}}]
                    | None -> None

                | None -> None

            | Addition (lhs, rhs) ->

                // Compute lhs and rhs column infos by recursively calling getColumnInfos
                // (IntLiteral or StringLiteral)
                let lhsColumnInfo = getColumnInfos (ResultExpr (lhs, None))
                let rhsColumnInfo = getColumnInfos (ResultExpr (rhs, None))

                // Return new column info list with columnName = "lhs column name" + "rhs column name"
                match lhsColumnInfo, rhsColumnInfo with
                | Some [lhsColumn], Some [rhsColumn] ->
                    let newColumnName : string =
                        match lhsColumn.columnName, rhsColumn.columnName with
                        | lhsName, rhsName -> lhsName + "+" + rhsName

                    Some [{ columnName = newColumnName; src = None }]

                | _ -> None

                // Same format for following Expr matches

            | Subtract (lhs, rhs) ->
                let lhsColumnInfo = getColumnInfos (ResultExpr (lhs, None))
                let rhsColumnInfo = getColumnInfos (ResultExpr (rhs, None))

                match lhsColumnInfo, rhsColumnInfo with
                | Some [lhsColumn], Some [rhsColumn] ->
                    let newColumnName : string =
                        match lhsColumn.columnName, rhsColumn.columnName with
                        | lhsName, rhsName -> lhsName + "-" + rhsName

                    Some [{ columnName = newColumnName; src = None }]

                | _ -> None

            | Multiply (lhs, rhs) ->
                let lhsColumnInfo = getColumnInfos (ResultExpr (lhs, None))
                let rhsColumnInfo = getColumnInfos (ResultExpr (rhs, None))

                match lhsColumnInfo, rhsColumnInfo with
                | Some [lhsColumn], Some [rhsColumn] ->
                    let newColumnName : string =
                        match lhsColumn.columnName, rhsColumn.columnName with
                        | lhsName, rhsName -> lhsName + "*" + rhsName

                    Some [{ columnName = newColumnName; src = None }]

                | _ -> None

            | Divide (lhs, rhs) ->
                let lhsColumnInfo = getColumnInfos (ResultExpr (lhs, None))
                let rhsColumnInfo = getColumnInfos (ResultExpr (rhs, None))

                match lhsColumnInfo, rhsColumnInfo with
                | Some [lhsColumn], Some [rhsColumn] ->
                    let newColumnName : string =
                        match lhsColumn.columnName, rhsColumn.columnName with
                        | lhsName, rhsName -> lhsName + "/" + rhsName

                    Some [{ columnName = newColumnName; src = None }]

                | _ -> None

            | Positive (ColumnReference columnName) ->
                
                // Find matching column infos in the original table based on columnName string
                let tryFindColumns : ColumnInfo option =
                    table.columnsInfo 
                    |> List.tryFind (fun columnInfo -> columnInfo.columnName = columnName)

                // If matching column infos found, return either "+(alias)" or "+(columnName)"
                // as ColumnInfo.columnName. Same format as Negative expr match
                match tryFindColumns with
                | Some columnInfo -> 
                    let exprColumnName =
                        match optAlias with
                        | Some aliasName -> "+" + aliasName
                        | _ -> "+" + columnName
                    Some [{columnName = exprColumnName; src = None}]
                | None -> None

            | Negative (ColumnReference columnName) ->
                let tryFindColumns : ColumnInfo option =
                    table.columnsInfo 
                    |> List.tryFind (fun columnInfo -> columnInfo.columnName = columnName)
                match tryFindColumns with
                | Some columnInfo -> 
                    let exprColumnName =
                        match optAlias with
                        | Some aliasName -> "-" + aliasName
                        | _ -> "-" + columnName
                    Some [{columnName = exprColumnName; src = None}]
                | None -> None

            | StringLiteral (stringQuery) ->
                Some [{columnName = stringQuery; src = None}] // Return new column info list with input string as columnName

            | IntLiteral (intQuery) ->
                Some [{columnName = string intQuery; src = None}] // Return new column info list with input int string as columnName

            | Parenthesis (subexpr) ->

                // Match computed sub-expression column infos in the parenthesis, 
                // then combine columnNames of the sub-expressions
                match getColumnInfos (ResultExpr (subexpr, None)) with
                | Some subExprColumns ->
                    let newExprColumns =
                        subExprColumns
                        |> List.map (fun columnInfo ->
                            { columnInfo with columnName = "(" + columnInfo.columnName + ")"})
                    Some newExprColumns
                | None -> None

            | _ -> None

    // Filter the column infos in the original table
    let selectColumns : ColumnInfo list =
        selectResults

        (* Get new lists of ColumnInfos based on selectResults.
        List.choose transforms items in a list into another list by
        applying a function then returning an option type *)
        |> List.choose getColumnInfos
        |> List.concat // Concatenate the lists into a single new list of ColumnInfos

    // Filter the rows in the original table
    let filteredRows =
        match selectResults with
        | [Star] -> table.rows // If result column in selectResults is Star, return unfiltered rows
        | _ -> // Else filter the rows
            table.rows
            // Create new row by mapping through the original rows
            |> Seq.map(fun row ->
                // Compute SQLValues in the row using selectResults result columns
                let selectedValues : SQLValue list = 
                    selectResults
                    |> List.map (fun resultCol ->
                        match resultCol with
                        | ResultExpr (expr, _) -> computeExprInRow table.columnsInfo row expr
                        | _ -> SQLNull )
                selectedValues // Return the list of SQLValues (which is the same as Row type)
            )

    { columnsInfo = selectColumns; rows = filteredRows; groups = table.groups }

// Perform Distinct clause
and performDistinct (distinct:bool) (table:Relation) : Relation =
    match distinct with
    | true -> 
        let distinctTable = table.rows |> Seq.distinct 
        {table with rows = distinctTable} // If distinct, return new table relation with distinct rows
    | false -> table // If not, return original table relation

// Perform Group By clause
and performGroupBy (groupbyColumns: Expr list option) (table:Relation): Relation =
    match groupbyColumns with
    | Some columns ->

        // Compute grouping key for each row
        let groupingKey row : SQLValue list =
            columns
            // Map expressions to SQLValues in a given row
            |> List.map (fun expr -> computeExprInRow table.columnsInfo row expr)

        // Group the rows based on the grouping key
        let groupRows : Row seq seq =
            table.rows
            |> Seq.groupBy groupingKey // Group the rows by the grouping key
            |> Seq.map (fun (key, rows) -> rows) // Get the grouped rows

        // Get a new list of column infos based on groupbyColumns
        let newColumnInfos : ColumnInfo list =
            columns
            |> List.choose (fun expr ->
                match expr with
                | ColumnReference columnName ->
                    table.columnsInfo
                    // Find matching column infos in the original table relation based on a ColumnReference columnName
                    |> List.tryFind (fun columnInfo -> columnInfo.columnName = columnName) 
                    // Returns the matching column infos and wraps it in a `Some` (due to List.choose function signature)
                    |> Option.map (fun originalColumnInfo -> { columnName = columnName; src = originalColumnInfo.src })
                | _ -> None )

        // Return grouped Relation
        { columnsInfo = newColumnInfos; rows = groupRows |> Seq.concat; groups = Some groupRows}

    | None -> table // If no group by, return original table relation

// Perform Having clause
and performHaving (condition:Expr option) (table:Relation): Relation =
    match condition with
    | Some expr ->

        // Filter the rows based on the having expression
        let filterRows : Row seq = 
            table.rows
            |> Seq.filter (fun row ->
                let boolResult = computeExprInRow table.columnsInfo row expr
                match boolResult with
                | SQLIntValue 1 -> true
                | _ -> false
            )
        // Pass the original table's rows into filterRows, return a new table with filtered rows
        {table with rows = filterRows}

    | None -> table // If no expression given, return original relation

// Perform OrderBy clause
and performOrderBy (orderbyexpressions: Expr list option) (table:Relation) : Relation =
    match orderbyexpressions with
    | Some expressions ->

        // Compare two rows (and their values) based on the order by expressions
        let compareRows row1 row2 : int =
            expressions
            // Compute the expressions on both rows
            |> List.map (fun expr -> computeExprInRow table.columnsInfo row1 expr, computeExprInRow table.columnsInfo row2 expr)
            
            (* Compare the values in the row
            The 'compareResult' accumulator stores the result of the comparisons (-1, 0, 1)
            -1 = (row1 < row2), 0 = (row1 = row2), 1 = (row1 > row2)

            -1 or 1 will determine how the rows are ordered in the final relation *)
            |> List.fold (fun compareResult (value1, value2) ->
                match compareResult, value1, value2 with
                | 0, SQLIntValue int1, SQLIntValue int2 -> compare int1 int2
                | 0, SQLVarcharValue str1, SQLVarcharValue str2 -> compare str1 str2
                | _ -> compareResult) 0 // Initial comparison result

        // Sort the rows based on the comparison
        let sortedRows : Row seq =
            table.rows
            |> Seq.sortWith compareRows

        // Return a new relation with the sorted rows
        { table with rows = sortedRows }

    | None -> table // If no expressions given, return original relation

// Perform Limit clause
and performLimit (limit: int option) (table:Relation) : Relation =
    match limit with
    | Some n ->
        let limitRows : Row seq =
            table.rows
            |> Seq.take n // Take the first n rows
        { table with rows = limitRows } // Return a new relation with limited rows
    | None -> table // If no limit integer given, return original relation

and Execute (ast:Query): Relation =
    performFrom ast.from
    |> performWhere ast.where
    |> performGroupBy ast.groupby
    |> performHaving ast.having
    |> performSelect ast.select
    |> performDistinct ast.distinct
    |> performOrderBy ast.orderby
    |> performLimit ast.limit



let Run (queryString:string) =
    queryString
    |> ParseSQLStmt 
    |> Execute


let Display queryString =
    do printfn "%s" queryString
    queryString
    |> Run
    |> RelationToString
    |> printfn "%s"