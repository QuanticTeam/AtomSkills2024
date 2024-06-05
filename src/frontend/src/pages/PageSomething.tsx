import { BreadcrumbProps, Button, Space, Table, TableProps, Tag } from 'antd'
import { Link, NavLink } from 'react-router-dom'
import { PageAuthorized } from '~/layouts/PageAuthorized'

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

const route = '/something'
const title = 'Все сущности'

export const breadcrumbs: BreadcrumbProps['items'] = [
  {
    title: (
      <NavLink
        to={route}
        className={({ isActive }) => (!isActive ? '!text-link-1' : '')}
        end
      >
        {title}
      </NavLink>
    ),
  },
]

export default function PageSomething() {
  return (
    <PageAuthorized
      breadcrumbs={breadcrumbs}
      title={title}
      actions={
        <Space>
          <Link to="/something/new">
            <Button type="primary">Создать</Button>
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
