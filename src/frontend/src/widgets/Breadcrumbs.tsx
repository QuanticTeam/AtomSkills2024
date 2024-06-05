import { Breadcrumb, BreadcrumbProps } from 'antd'

export interface BreadcrumbsProps {
  breadcrumbs: BreadcrumbProps['items']
}

export function Breadcrumbs({ breadcrumbs = [] }: BreadcrumbsProps) {
  return (
    <Breadcrumb
      items={[
        {
          title: '',
        },
        ...breadcrumbs,
      ]}
      className="p-6"
    />
  )
}
