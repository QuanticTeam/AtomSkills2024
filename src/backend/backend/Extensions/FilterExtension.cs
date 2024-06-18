using System.Linq.Expressions;
using backend.Contracts;

namespace backend.Extensions;

public static class FilterExtension
{
    public static Expression<Func<TSource, bool>> Filter<TSource>(List<Filter> filters)
    {
        var expression = ConstFilter<TSource>.True;

        foreach (var filter in filters)
        {
            var columnName = filter.ColumnName.ToFirstLetterUpper();
            
            var type = typeof(TSource).GetProperty(columnName);
            
            if (type == null)
                continue;
            
            var typeExpression = type.PropertyType.Name switch
            {
                "String" => StringFilter<TSource>(columnName, filter.Values.Single(), filter.FilterType),
                "Int" => IntFilter<TSource>(columnName, int.Parse(filter.Values.Single()), filter.FilterType),
                "Double" => DoubleFilter<TSource>(columnName, double.Parse(filter.Values.Single()), filter.FilterType),
                "DateTime" => DateTimeFilter<TSource>(columnName, DateTime.Parse(filter.Values.Single()).ToUniversalTime(), filter.FilterType),
                _ => throw new ArgumentOutOfRangeException(nameof(type), type, "Filter does not exist")
            };
            
            expression = expression.AndAlso(typeExpression);
        }

        return expression;
    }
    
    private static Expression<Func<TSource, bool>> AndAlso<TSource>(this Expression<Func<TSource, bool>> expr1, Expression<Func<TSource, bool>> expr2)
    {
        if (expr2 == ConstFilter<TSource>.True)
        {
            return expr1;
        }

        if (expr1 == ConstFilter<TSource>.True)
        {
            return expr2;
        }

        var parameter = Expression.Parameter(typeof(TSource));

        var left = expr1.Body.Replace(expr1.Parameters[0], parameter);

        var right = expr2.Body.Replace(expr2.Parameters[0], parameter);

        return Expression.Lambda<Func<TSource, bool>>(Expression.AndAlso(left, right), parameter);
    }
    
    private static Expression Replace(this Expression body, Expression one, Expression another) => new Replacer(one, another).Visit(body);
    
    private class Replacer(Expression one, Expression another) : ExpressionVisitor
    {
        public override Expression Visit(Expression node)
        {
            return node == one ? another : base.Visit(node);
        }
    }
    
    private static class ConstFilter<TSource>
    {
        public static readonly Expression<Func<TSource, bool>> True = x => true;
    }
    
    private static Expression<Func<TSource, bool>> StringFilter<TSource>(string propertyName, string value, FilterType type)
    {
        var parameter = Expression.Parameter(typeof(TSource), "x");
        
        var property = Expression.Property(parameter, propertyName);
        
        var constant = Expression.Constant(value);
        
        var method = type switch
        {
            FilterType.Equals => typeof(string).GetMethod("Equals", new[] { typeof(string) }),
            FilterType.Contains => typeof(string).GetMethod("Contains", new[] { typeof(string) }),
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, "Filter does not exist")
        };
        
        var expression = Expression.Call(property, method!, constant);
        
        return Expression.Lambda<Func<TSource, bool>>(expression, parameter);
    }
    
    private static Expression<Func<TSource, bool>> IntFilter<TSource>(string propertyName, int value, FilterType type)
    {
        var parameter = Expression.Parameter(typeof(TSource), "x");
        
        var property = Expression.Property(parameter, propertyName);
        
        var constant = Expression.Constant(value);
        
        var expression = type switch
        {
            FilterType.Equals => Expression.Equal(property, constant),
            FilterType.GreaterThan => Expression.GreaterThan(property, constant),
            FilterType.LessThan => Expression.LessThan(property, constant),
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, "Filter does not exist")
        };
        
        return Expression.Lambda<Func<TSource, bool>>(expression, parameter);
    }
    
    private static Expression<Func<TSource, bool>> DoubleFilter<TSource>(string propertyName, double value, FilterType type)
    {
        var parameter = Expression.Parameter(typeof(TSource), "x");
        
        var property = Expression.Property(parameter, propertyName);

        if (type == FilterType.Contains)
        {
                var constant2 = Expression.Constant(value.ToString());
                var method2 = typeof(string).GetMethod("Contains", new[] { typeof(string) });
                var expression2 = Expression.Call(property, method2!, constant2);
                return Expression.Lambda<Func<TSource, bool>>(expression2, parameter);
        }

        var constant = Expression.Constant(value);
        
        var expression = type switch
        {
            FilterType.Equals => Expression.Equal(property, constant),
            FilterType.GreaterThan => Expression.GreaterThan(property, constant),
            FilterType.LessThan => Expression.LessThan(property, constant),
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, "Filter does not exist")
        };

        return Expression.Lambda<Func<TSource, bool>>(expression, parameter);
    }

    private static Expression<Func<TSource, bool>> DateTimeFilter<TSource>(string propertyName, DateTime value, FilterType type)
    {
        var parameter = Expression.Parameter(typeof(TSource), "x");
        var property = Expression.Property(parameter, propertyName);

        if (type == FilterType.Contains)
        {
                var constant2 = Expression.Constant(value.ToString());
                var method2 = typeof(string).GetMethod("Contains", new[] { typeof(string) });
                var expression2 = Expression.Call(property, method2!, constant2);
                return Expression.Lambda<Func<TSource, bool>>(expression2, parameter);
        }

        var constant = Expression.Constant(value);
        var expression = type switch
        {
            FilterType.Equals => Expression.Equal(property, constant),
            FilterType.GreaterThan => Expression.GreaterThan(property, constant),
            FilterType.LessThan => Expression.LessThan(property, constant),
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, "Filter does not exist")
        };

        return Expression.Lambda<Func<TSource, bool>>(expression, parameter);
    }
}