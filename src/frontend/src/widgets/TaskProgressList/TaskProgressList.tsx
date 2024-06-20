import { SearchOutlined, StarOutlined, StarTwoTone } from '@ant-design/icons'
import { useQuery } from '@tanstack/react-query'
import {
  Badge,
  Button,
  Input,
  InputRef,
  Popover,
  Space,
  Spin,
  Table,
  TableColumnType,
  TableProps,
  Typography,
} from 'antd'
import { FilterDropdownProps } from 'antd/es/table/interface'
import { useState, useEffect, useRef, useContext } from 'react'
import { useTranslation } from 'react-i18next'
import { Link, useParams } from 'react-router-dom'
import { Lesson, LessonsApi, Task, TaskProgress, TaskStatusType, TasksApi } from '~/entities'
import { SortAndFilterRequest, getSortAndFilterRequestPayload, FilterType } from '~/shared/api'
import { AuthContext } from '~/shared/auth'
import { ROUTE_PATH_LESSONS } from '~/shared/routing'
import { Condition, Markdown } from '~/shared/ui'

type DataIndex = keyof TaskProgress

export function TaskProgressList() {
  const { lessonCode, taskCode: code, userId: _userId } = useParams<Record<string, string>>()
  const { t } = useTranslation(TaskProgressList.name)
  const { authInfo } = useContext(AuthContext)

  const isMentorOnReview = authInfo?.user.isMentor && _userId
  const isMentorOnLookup = authInfo?.user.isMentor && !_userId

  const isStudentOnLookup = authInfo?.user.isStudent
  const isAdmin = authInfo?.user.isAdmin

  const [searchText, setSearchText] = useState('')
  const [searchedColumn, setSearchedColumn] = useState('')
  const searchInput = useRef<InputRef>(null)

  const handleSearch = (
    selectedKeys: string[],
    confirm: FilterDropdownProps['confirm'],
    dataIndex: DataIndex,
  ) => {
    confirm()
    setSearchText(selectedKeys[0])
    setSearchedColumn(dataIndex)
  }

  const handleReset = (clearFilters: () => void, confirm: () => void) => {
    clearFilters()
    setSearchText('')
    setSearchedColumn('')
    confirm()
  }

  const getColumnSearchProps = (dataIndex: DataIndex): TableColumnType<TaskProgress> => ({
    filterDropdown: ({ setSelectedKeys, selectedKeys, confirm, clearFilters, close }) => (
      <div
        style={{ padding: 8 }}
        onKeyDown={e => e.stopPropagation()}
      >
        <Input
          ref={searchInput}
          placeholder={`Search ${dataIndex}`}
          value={selectedKeys[0]}
          onChange={e => setSelectedKeys(e.target.value ? [e.target.value] : [])}
          onPressEnter={() => handleSearch(selectedKeys as string[], confirm, dataIndex)}
          style={{ marginBottom: 8, display: 'block' }}
        />
        <Space>
          <Button
            type="primary"
            onClick={() => handleSearch(selectedKeys as string[], confirm, dataIndex)}
            icon={<SearchOutlined />}
            size="small"
            style={{ width: 90 }}
          >
            Поиск
          </Button>
          <Button
            onClick={() => clearFilters && handleReset(clearFilters, confirm)}
            size="small"
            style={{ width: 90 }}
          >
            Сбросить
          </Button>
        </Space>
      </div>
    ),
    filterIcon: (filtered: boolean) => (
      <SearchOutlined style={{ color: filtered ? '#1677ff' : undefined }} />
    ),
    // onFilter: (value, record) =>
    //   record[dataIndex]
    //     .toString()
    //     .toLowerCase()
    //     .includes((value as string).toLowerCase()),
    onFilterDropdownOpenChange: visible => {
      if (visible) {
        setTimeout(() => searchInput.current?.select(), 100)
      }
    },
    // render: text =>
    //   searchedColumn === dataIndex ? (
    //     <SearchHighlighter
    //       searchWords={[searchText]}
    //       textToHighlight={text}
    //     />
    //   ) : (
    //     text
    //   ),
  })

  const columns: TableProps<TaskProgress>['columns'] = [
    {
      title: 'Пользователь',
      dataIndex: 'userKey',
      key: 'userKey',
      hidden: !!isMentorOnReview,
      // ...getColumnSearchProps('code'),
    },
    {
      title: 'Взято в работу',
      dataIndex: 'startedAt',
      key: 'startedAt',
      render(value) {
        if (!value) return ''
        return new Date(value).toLocaleString()
      },
      // ...getColumnSearchProps('title'),
      // render(value, record) {
      //   return (
      //     <Link to={`${ROUTE_PATH_LESSONS}/${lessonCode}/tasks/${record.code}`}>
      //       <Typography.Link>{value}</Typography.Link>
      //     </Link>
      //   )
      // },
    },
    {
      title: 'Завершено',
      dataIndex: 'finishedAt',
      key: 'finishedAt',
      render(value) {
        if (!value) return ''
        return new Date(value).toLocaleString()
      },
      // ...getColumnSearchProps('content'),
      // render(value) {
      //   return (
      //     <Popover
      //       trigger="click"
      //       content={<Markdown markdown={value} />}
      //       placement="right"
      //       overlayInnerStyle={{
      //         width: '40vw',
      //         height: '40vh',
      //         overflow: 'auto',
      //       }}
      //     >
      //       <Typography.Link className="border-b  border-dashed border-blue-600">
      //         {value.split(' ').slice(0, 10).join(' ') + '...'}
      //       </Typography.Link>
      //     </Popover>
      //   )
      // },
    },
    // {
    //   title: t('colSupplement'),
    //   dataIndex: 'supplements',
    //   key: 'supplements',
    //   render(value) {
    //     return JSON.stringify(value)
    //   },
    // },
    {
      title: 'Статус',
      dataIndex: 'status',
      // ...getColumnSearchProps('difficulty'),
      key: 'status',
      render(status) {
        return (
          <>
            <Condition conditions={[() => status === TaskStatusType.None]}>
              <Badge
                status="default"
                text="Новое"
              />
            </Condition>
            <Condition conditions={[() => status === TaskStatusType.InWork]}>
              <Badge
                status="processing"
                text="Выполняется"
              />
            </Condition>
            <Condition conditions={[() => status === TaskStatusType.SendToCheck]}>
              <Badge
                status="processing"
                color="orange"
                text="На проверке"
              />
            </Condition>
            <Condition conditions={[() => status === TaskStatusType.AiVerified]}>
              <Badge
                status="success"
                text="Проверено AI"
              />
            </Condition>
            <Condition conditions={[() => TaskStatusType.Verified === status]}>
              <Badge
                status="success"
                text="Проверено"
              />
            </Condition>
            <Condition conditions={[() => TaskStatusType.Recommended === status]}>
              <Badge
                status="warning"
                text="Рекомендация перепройти"
              />
            </Condition>
          </>
        )
      },
    },
    {
      title: 'Оценка',
      dataIndex: 'mark',
      key: 'mark',
      render(value) {
        return Array.from({ length: 5 }).map((_, i) => {
          if (!value) return <StarOutlined className="text-slate-400" />
          return i + 1 <= value ? <StarTwoTone /> : <StarOutlined className="text-slate-400" />
        })
      },
      // ...getColumnSearchProps('time'),
    },
    {
      title: 'Результат',
      dataIndex: 'photos',
      key: 'photos',
      // ...getColumnSearchProps('time'),
    },
    {
      title: 'Рекоммендации',
      dataIndex: 'recommendations',
      key: 'recommendations',
      // ...getColumnSearchProps('time'),
    },
    {
      title: 'Действия',
      hidden: !isMentorOnLookup,
      key: 'actions',
      render(_, record) {
        return (
          <Space>
            <Link
              to={`/lessons/${lessonCode}/tasks/${code}/users/${record.userKey}`}
              target="_blank"
            >
              <Typography.Link>Просмотреть</Typography.Link>
            </Link>
          </Space>
        )
      },
      // ...getColumnSearchProps('time'),
    },
  ]

  const [sortAndFilterPayload, setSortAndFilterPayload] = useState<SortAndFilterRequest>(
    getSortAndFilterRequestPayload(),
  )

  const { data, error, isPending, refetch } = useQuery({
    queryKey: ['taskProgress', code],
    queryFn: () => TasksApi.getTaskProgress(code!),
  })

  useEffect(() => {
    refetch()
  }, [refetch, sortAndFilterPayload])

  if (isPending) return <Spin />

  const onTableChange: TableProps<TaskProgress>['onChange'] = (
    pagination,
    filters,
    sorter,
    extra,
  ) => {
    switch (extra.action) {
      case 'sort': {
        if (!Array.isArray(sorter)) {
          console.log(sorter)
          setSortAndFilterPayload(prev => ({
            ...prev,
            orderBy: sorter.order ? (sorter.columnKey as string) : '',
            descending: sorter.order === ('descend' as string),
          }))
        }

        break
      }

      case 'filter': {
        setSortAndFilterPayload(prev => ({
          ...prev,
          filters: Object.entries(filters)
            .filter(x => x[1] && x[1].length)
            .map(([columnName, values]) => ({
              columnName,
              values: (values as string[]) ?? [],
              filterType: FilterType.Contains,
            })),
        }))

        break
      }
    }

    console.log('params', { pagination, filters, sorter, extra })
  }

  data?.sort((a, b) => {
    if (!a.startedAt) return -1
    if (!b.startedAt) return -1

    return Date.parse(b.startedAt) - Date.parse(a.startedAt)
  })

  return (
    <Table
      bordered
      columns={columns}
      dataSource={
        isStudentOnLookup ? data?.filter(x => x.userKey === authInfo.tokenPayload.userId) : data
      }
      pagination={false}
      onChange={onTableChange}
      scroll={{
        x: 0,
        y: 600,
      }}
    />
  )
}
