import { BreadcrumbProps, Button, Space, Table, TableProps, Tag } from 'antd'
import { t } from 'i18next'
import { useTranslation } from 'react-i18next'
import { Link, NavLink } from 'react-router-dom'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { ROUTE_PATH_SOMETHING, ROUTE_PATH_SOMETHING_NEW } from '~/shared/routing'

interface DataType {
  key: string
  name: string
  age: number
  address: string
  tags: string[]
}

const columns: TableProps<DataType>['columns'] = [
  {
    title: 'Name',
    dataIndex: 'name',
    key: 'name',
    render: text => <a>{text}</a>,
  },
  {
    title: 'Age',
    dataIndex: 'age',
    key: 'age',
  },
  {
    title: 'Address',
    dataIndex: 'address',
    key: 'address',
  },
  {
    title: 'Tags',
    key: 'tags',
    dataIndex: 'tags',
    render: (_, { tags }) => (
      <>
        {tags.map(tag => {
          let color = tag.length > 5 ? 'geekblue' : 'green'
          if (tag === 'loser') {
            color = 'volcano'
          }
          return (
            <Tag
              color={color}
              key={tag}
            >
              {tag.toUpperCase()}
            </Tag>
          )
        })}
      </>
    ),
  },
  {
    title: 'Action',
    key: 'action',
    render: (_, record) => (
      <Space size="middle">
        <a>Invite {record.name}</a>
        <a>Delete</a>
      </Space>
    ),
  },
]

const data: DataType[] = [
  {
    key: '1',
    name: 'John Brown',
    age: 32,
    address: 'New York No. 1 Lake Park',
    tags: ['nice', 'developer'],
  },
  {
    key: '2',
    name: 'Jim Green',
    age: 42,
    address: 'London No. 1 Lake Park',
    tags: ['loser'],
  },
  {
    key: '3',
    name: 'Joe Black',
    age: 32,
    address: 'Sydney No. 1 Lake Park',
    tags: ['cool', 'teacher'],
  },
]

export const breadcrumbs: BreadcrumbProps['items'] = [
  {
    title: (
      <NavLink
        to={ROUTE_PATH_SOMETHING}
        className={({ isActive }) => (!isActive ? '!text-link-1' : '')}
        end
      >
        {t('AllEntities')}
      </NavLink>
    ),
  },
]

export default function PageSomething() {
  const { t } = useTranslation()

  return (
    <PageAuthorized
      breadcrumbs={breadcrumbs}
      title={t('All entities')}
      actions={
        <Space>
          <Link to={ROUTE_PATH_SOMETHING_NEW}>
            <Button type="primary">{t('Create')}</Button>
          </Link>
        </Space>
      }
    >
      <Table
        columns={columns}
        dataSource={data}
      />
    </PageAuthorized>
  )
}
