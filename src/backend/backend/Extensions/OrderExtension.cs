using System.Linq.Expressions;
using backend.Core.Models;

namespace backend.Extensions;

public static class OrderExtension
{
    public static IQueryable<TSource> Order<TSource>(this IQueryable<TSource> source,string columnName, bool descending)
    {
        var command = descending ? "OrderByDescending" : "OrderBy";
        
        var property = typeof(TSource).GetProperty(columnName.ToFirstLetterUpper());
        
        var parameter = Expression.Parameter(typeof(TSource), "p");
        
        var propertyAccess = Expression.MakeMemberAccess(parameter, property!);
        
        var orderByExpression = Expression.Lambda(propertyAccess, parameter);
        
        var resultExpression = Expression.Call(typeof(Queryable), command, new Type[] { typeof(TSource), property!.PropertyType },
            source.Expression, Expression.Quote(orderByExpression));
        
        return source.Provider.CreateQuery<TSource>(resultExpression);
    }
}