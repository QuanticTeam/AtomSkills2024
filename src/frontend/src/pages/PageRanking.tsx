import { QuestionCircleOutlined } from '@ant-design/icons'
import { faker } from '@faker-js/faker'
import { useQuery } from '@tanstack/react-query'
import { App, Button, Popover, Space, Spin, Table, TableProps, Typography } from 'antd'
import { AxiosError } from 'axios'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { apiClient } from '~/shared/apiClient'

export default function PageRanking() {
  const { data } = useQuery({
    queryKey: ['rank'],
    queryFn: async () => (await apiClient.post('/Rank/RankTasks')).data.rankTasks,
  })

  if (!data) return <Spin />

  const columns: TableProps<any>['columns'] = [
    {
      title: (
        <>
          Рейтинг{' '}
          <Popover
            content={
              <Typography.Text>
                1000 * средний балл минус максимальное время прохождения
              </Typography.Text>
            }
          >
            <QuestionCircleOutlined className="text-blue-500" />
          </Popover>
        </>
      ),
      dataIndex: 'rank',
      key: 'rank',
    },
    {
      title: 'Код',
      dataIndex: 'code',
      key: 'code',
    },
    {
      title: 'Заголовок',
      dataIndex: 'title',
      key: 'title',
    },
    {
      title: 'Число двоечников',
      dataIndex: 'countTwo',
      key: 'countTwo',
    },
    {
      title: 'Число троечников',
      dataIndex: 'countThree',
      key: 'countThree',
    },
    {
      title: 'Число хорошистов',
      dataIndex: 'countFour',
      key: 'countFour',
    },
    {
      title: 'Число отличников',
      dataIndex: 'countFive',
      key: 'countFive',
    },
  ]

  return (
    <PageAuthorized title="Рейтинг по заданиям">
      <Table
        columns={columns}
        dataSource={data}
        pagination={false}
      />
    </PageAuthorized>
  )
}
