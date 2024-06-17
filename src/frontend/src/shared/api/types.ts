export function getSortAndFilterRequestPayload({
  orderBy = '',
  descending = true,
  filters = [],
}: SortAndFilterRequest = {}) {
  return {
    orderBy,
    descending,
    filters,
  }
}

export interface SortAndFilterRequest {
  orderBy?: string
  descending?: boolean
  filters?: Filter[]
}

interface Filter {
  columnName: string
  // всегда один элемент массива для простых фильтров
  values: string[]
  filterType: FilterType
}

export enum FilterType {
  Contains = 0, // substring
  Equals = 1,
  GreaterThan = 2,
  LessThan = 3,
  OneOf = 4, // для фильтрации по меткам (теги, темы и т.п.)
}
