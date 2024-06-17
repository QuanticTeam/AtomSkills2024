import { Spin, Table, TableProps, Typography } from 'antd'
import { useTranslation } from 'react-i18next'
import { Link } from 'react-router-dom'
import { Lesson, Task } from '~/entities'
import { ROUTE_PATH_TASKS } from '~/shared/routing'

export interface TasksProps {
  tasks: Task[]
}

export function Tasks({ tasks }: TasksProps) {
  const { t } = useTranslation(Tasks.name)

  const columns: TableProps<Task>['columns'] = [
    {
      title: t('colCode'),
      dataIndex: 'code',
      key: 'code',
    },
    {
      title: t('colTitle'),
      dataIndex: 'title',
      key: 'title',
      render(value, record) {
        return (
          <Link to={`${ROUTE_PATH_TASKS}/${record.code}`}>
            <Typography.Link>{value}</Typography.Link>
          </Link>
        )
      },
    },
    {
      title: t('colContent'),
      dataIndex: 'content',
      key: 'content',
    },
    {
      title: t('colDifficulty'),
      dataIndex: 'difficulty',
    },
    {
      title: t('colSupplement'),
      dataIndex: 'supplements',
      key: 'supplements',
      render(value) {
        return JSON.stringify(value)
      },
    },
    {
      title: t('colTasks'),
      dataIndex: 'tasks',
      key: 'tasks',
      render(value) {
        return JSON.stringify(value)
      },
    },
    {
      title: t('colAuthor'),
      dataIndex: 'author',
      key: 'author',
    },
  ]

  return (
    <Table
      columns={columns}
      dataSource={tasks}
      pagination={false}
      scroll={{
        x: 0,
        y: 500,
      }}
    />
  )
}
