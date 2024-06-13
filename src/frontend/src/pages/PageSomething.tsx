import { BreadcrumbProps, Button, Space, Table, TableProps, Tag } from 'antd'
import { t } from 'i18next'
import { useEffect, useState } from 'react'
import { useTranslation } from 'react-i18next'
import { Link, NavLink } from 'react-router-dom'
import { Something, SomethingApi } from '~/entities/Something'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { ROUTE_PATH_SOMETHING, ROUTE_PATH_SOMETHING_NEW } from '~/shared/routing'

const columns: TableProps<Something>['columns'] = [
  {
    title: 'Key',
    dataIndex: 'key',
    key: 'key',
  },
  {
    title: 'Name',
    dataIndex: 'name',
    key: 'name',
  },
  {
    title: 'Integer',
    dataIndex: 'integer',
    key: 'integer',
  },
  {
    title: 'Number',
    dataIndex: 'number',
    key: 'number',
  },
  {
    title: 'DateTime',
    dataIndex: 'dateTime',
    key: 'dateTime',
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
  const [data, setData] = useState<Something[]>([])

  useEffect(() => {
    async function fetch() {
      const result = await SomethingApi.fetch()
      setData(result)
    }

    fetch()
  }, [])

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
