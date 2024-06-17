import { Table, TableProps, Typography } from 'antd'
import { useTranslation } from 'react-i18next'
import { Link } from 'react-router-dom'
import { Lesson, Task } from '~/entities'
import { ROUTE_PATH_LESSONS } from '~/shared/routing'

export interface TasksProps {
  tasks: Task[]
  lessonCode: Lesson['code']
}

export function Tasks({ tasks, lessonCode }: TasksProps) {
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
          <Link to={`${ROUTE_PATH_LESSONS}/${lessonCode}/tasks/${record.code}`}>
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
      title: t('colSupplement'),
      dataIndex: 'supplements',
      key: 'supplements',
      render(value) {
        return JSON.stringify(value)
      },
    },
    {
      title: t('colDifficulty'),
      dataIndex: 'difficulty',
      key: 'difficulty',
    },
    {
      title: t('colTime'),
      dataIndex: 'time',
      key: 'time',
      render(value) {
        return JSON.stringify(value)
      },
    },
  ]

  return (
    <Table
      bordered
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
