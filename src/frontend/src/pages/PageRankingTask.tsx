import { QuestionCircleOutlined } from '@ant-design/icons'
import { faker } from '@faker-js/faker'
import { useQuery } from '@tanstack/react-query'
import { App, Button, Popover, Space, Spin, Table, TableProps, Typography } from 'antd'
import { AxiosError } from 'axios'
import { useParams } from 'react-router-dom'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { apiClient } from '~/shared/apiClient'

export default function PageRankingTask() {
  const { code } = useParams()

  const { data } = useQuery({
    queryKey: ['rank', code],
    queryFn: async () => (await apiClient.post('/Rank/RankUsers', { code })).data.userRanks,
  })

  if (!data) return <Spin />

  const columns: TableProps<any>['columns'] = [
    {
      title: 'Имя',
      dataIndex: 'name',
      key: 'name',
    },
    {
      title: 'Время (мин)',
      dataIndex: 'minutes',
      key: 'minutes',
    },
    {
      title: 'Оценка',
      dataIndex: 'mark',
      key: 'mark',
    },
  ]

  return (
    <PageAuthorized
      title={
        <Typography.Title>
          Рейтинг по заданию (код {code}){' '}
          <Popover
            content={
              <Typography.Text>
                Рейтинг строится за счет сортировки по убыванию по оценке, а затем по возрастанию по
                затраченному времени
              </Typography.Text>
            }
          >
            <QuestionCircleOutlined className="text-blue-500" />
          </Popover>
        </Typography.Title>
      }
    >
      <Table
        columns={columns}
        dataSource={data}
        pagination={false}
      />
    </PageAuthorized>
  )
}
