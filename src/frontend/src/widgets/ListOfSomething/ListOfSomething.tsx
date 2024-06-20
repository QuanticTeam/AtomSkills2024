import { useQuery } from '@tanstack/react-query'
import { Spin, Table, TableProps } from 'antd'
import dayjs from 'dayjs'
import { Ref } from 'react'
import { useTranslation } from 'react-i18next'
import { Something, SomethingApi } from '~/entities'
import { Oops } from '~/shared/ui'

interface ListOfSomethingProps {
  parentRef?: Ref<HTMLDivElement>
}

export function ListOfSomething({ parentRef }: ListOfSomethingProps) {
  const { t } = useTranslation(ListOfSomething.name)

  const columns: TableProps<Something>['columns'] = [
    {
      title: t('colKey'),
      dataIndex: 'key',
      key: 'key',
      width: '20%',
    },
    {
      title: t('colName'),
      dataIndex: 'name',
      key: 'name',
      width: '20%',
    },
    {
      title: t('colInteger'),
      dataIndex: 'integer',
      key: 'integer',
      width: '20%',
    },
    {
      title: t('colNumber'),
      dataIndex: 'number',
      key: 'number',
      width: '20%',
    },
    {
      title: t('colDateTime'),
      dataIndex: 'dateTime',
      key: 'dateTime',
      render(_, record) {
        return dayjs(record.dateTime).format('DD-MM-YYYY HH:mm:ss')
      },
      width: '20%',
    },
  ]

  const { data, error, isPending } = useQuery({
    queryKey: ['something'],
    queryFn: SomethingApi.fetch,
  })

  return isPending ? (
    <Spin />
  ) : error ? (
    <Oops />
  ) : (
    <Table
      style={{ height: '100%' }}
      columns={columns}
      dataSource={data}
      pagination={false}
      scroll={{
        x: 0,
        y: 600,
      }}
    />
  )
}
